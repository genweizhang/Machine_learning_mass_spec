

% Create the tissue label
% Tissue_label=clipboard('pastespecial')
Tissue_label=zeros(14100,1)
% 0: inner layer , 1: middle layer, 2:outer layer
for i=3001:10500
    Tissue_label(i,1)=1;
end
for i=10501:14100
    Tissue_label(i,1)=2;
end
% By default, it i using barnes-hut approximation method and PCA 50 to
% decompose.
Y=tsne(aligned_intensity_matrix)
% test first 20 lines
Y1=tsne(aligned_intensity_matrix_20)

% adjusting t-sne parameters
no_dims= 2;
initial_dims= 50;
perplexity= 30;
% Run t-SNE
% (1)Dr. Yang's script tsne:

Y2=tsne(aligned_intensity_matrix_20,no_dims,initial_dims,perplexity);

% Plot
gscatter (Y(:,1),Y(:,2),Tissue_label)

% comparison between arbitrarily assigned label and ML generated label
gscatter (Y(:,1),Y(:,2),machinelearninggeneratedlabelfull2)
gscatter (Y(:,1),Y(:,2),Kidneytissuesectionlabel_number)





% (2) Matlab built-in tsne:

Y1= tsne(aligned_intensity_matrix_20,'NumPrint',10,'options',statset('MaxIter',5000),...
        'perplexity',30,'NumDimensions',2,'NumPCAComponents',50,'Verbose',1,...
        'LearnRate',500,'Exaggeration',6);
gscatter (Y1(:,1),Y1(:,2),machinelearninggeneratedlabel);





% OPTICS, odering objects
minpts=20;
epsilon=0.0200;
[SetOfClusters,RD1,CD1,order1] = cluster_optics(Y, minpts, epsilon)
bar(RD1(order1));

figure;
% Cycle through all clusters
for i=2:length(SetOfClusters)
    bar(RD1(order1(SetOfClusters(i).start:SetOfClusters(i).end)));
    pause(2)
end

% visualize the results
bar(order1,RD1)

% DBSCAN
% Scaling Y to between -1 and 1.

scaledY=[];
scaledY = 2*( (Y-min(Y(:))) ./ (max(Y(:)-min(Y(:)))))-1;

minpts=50;
epsilon=0.04;
[IDY,isnoise]=DBSCAN(scaledY,epsilon,minpts)

% Plot Results (clusters)

PlotClusterinResult(scaledY, IDY);
title(['DBSCAN Clustering (\epsilon = ' num2str(epsilon) ', MinPts = ' num2str(minpts) ')']);




