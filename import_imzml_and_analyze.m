% Title: Basic Matlab preprocessing script for imzML format files
%
% Author: Genwei Zhang, Xiang Tian,Yihan Shao, Zhibo Yang, University of Oklahoma, 2017.
%
% This program is free software: you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation, either version 3 of the License, or
%     (at your option) any later version.
% 
%     This program is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.
% 
%     You should have received a copy of the GNU General Public License
%     along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

clc;
% Import the imzMLConverter (choose the folder)
% add JAR file to the Java class path:
javaclasspath /Users/Genwei/Desktop/'Dr. Shao and Yang'/'MS Data'/imzMLConverter.jar;
filename='/Users/Genwei/Desktop/Dr. Shao and Yang/Xiang/spheroid control imzML/spheroid control.imzML';
imzML=imzMLConverter.ImzMLHandler.parseimzML(filename);
nColumns=imzML.getWidth();
nRows=imzML.getHeight();

% Savitzky-Golay parameters
SPAN = 7;
DEGREE = 2;

% SNR threshold filter
SNR_THRES = 5;

fprintf('...importing raw\n');
raw_peaks = cell(sum(nRows .* nColumns), 1);
% aa= 1;
% Matlab is column-wise, so y is row numbers, x is column numbers.
% peaklist={};
j=1;
for x = 1:nColumns
    for y = 1:nRows
        % Read the m/z vector and ion intensities
        mz_vector = imzML.getSpectrum(x,y).getmzArray();
        int_vector = imzML.getSpectrum(x,y).getIntensityArray();
        int_vector(int_vector < 0) = 0; % Remove all the negative values
        
        % Perform the smoothing
        smoothed_int = mssgolay(mz_vector, int_vector, 'Span', SPAN, 'Degree', DEGREE);
        
        % Peak detection (customize the parameters)
        peak_list = mspeaks(mz_vector, smoothed_int);
        
        % If you want to apply SNR based filter
        snr_ = 1.4826 * mad(peak_list(:, 2), 1);
        mask = peak_list(:, 2) >= SNR_THRES * snr_;
        peak_list = peak_list(mask, :);
        
        % take log/e transformation
        peak_list(:, 2)=log(peak_list(:, 2))
        % Assign the peaks to the cell array element
        
        raw_peaks{j} = peak_list; % Each cell contains two columns matrix: masses, intensities#
       
        % Increase the index
        j = j+1;
    end
end
fprintf('done\n');

% or perform the log transformation after created the raw peaks
for j=1:14100
    raw_peaks{j}(:,2)=log(raw_peaks{j}(:,2))
end
% Peak matching
[CMZ, aligned_peaks] = mspalign(raw_peaks, 'EstimationMethod', 'histogram', ...
    'CorrectionMethod', 'nearest-neighbor');

% Generate intensity matrix/ plot the mass spec heatmap!
%{
YY75aligned=[];
parfor i=1:14100
    
   % Using optimized 1195 m/z, no resultion lost! 
   [MASS,YY]=msppresample(aligned_peaks{i},1000)
   % horizontal concatenate
   YY75aligned=horzcat(YY75aligned,YY.')
end
% Heatmap aligned peaks by sampling 1000 m/z and then plot!!!
msheatmap(MASS, log(YY75aligned)) 
%}

% Perform PCA 
[coeff1,score1]=pca(aligned_intensity_matrix,'NumComponents',2)
[coeff2,score2]=pca(aligned_intensity_matrix,'NumComponents',3)

% plot
scatter(score1(:,1),score1(:,2),'.')

scatter3(score2(:,1),score2(:,2),score2(:,3),'.')

% Fill the intensity matrix
fprintf('...generating the intensity matrix\n');
aligned_intensity_matrix_644 = [];
parfor i = 1:14100
    n=double(ismember(CMZ, aligned_peaks{i}(:, 1)))
    a=1
    Y=zeros(1,644);
    for j=1:length(CMZ)
  
        if n(j)==1 && a<=length(aligned_peaks{i})
                    Y(j) = aligned_peaks{i}(a,2)
                    a=a+1;
        else
        end
        
    end
    aligned_intensity_matrix_644(i,:)=Y
end
fprintf('done\n');
msheatmap(CMZ,log(aligned_intensity_matrix))