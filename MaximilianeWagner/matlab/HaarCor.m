
clear all

%% read data
load JoinmitCortisolohneSIC.txt;
dat=JoinmitCortisolohneSIC(:,3:end);

%% preprocess data

% delete 3 largest cortisol values (Cortisol > 50)
i = find(dat(:,1)<50);
dat1 = dat(i,:);

% delete BMI_ori (because closely related to BMI_adj)
dat2 = [dat1(:,1:12) dat1(:,14:16)]; 

% use log of cortisol
dat2(:,1)=log(dat2(:,1));
% % distribution of cortisol
% figure
% boxplot(dat2(:,1))

% % scatter plots of all variables 
figure
plotmatrix(dat2)
print(gcf,'-djpeg','hair_scatter.jpg')
%title(tit,'FontWeight','bold')

%% correlation between variables 

% Spearman rank correlation
[rho,p]=corr(dat2,'rows','pairwise','type','Spearman');
% Pearson linear correlation
[rho1,p1]=corr(dat2,'rows','pairwise','type','Pearson');
% write correlation coefficient and significance to file
% fid = fopen('hair_corrPearson.dat','wt');
% fprintf(fid,'%5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e\n',rho');
% fclose(fid);
% fid = fopen('hair_corrPearsonSig.dat','wt');
% fprintf(fid,'%5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e\n',p');
% fclose(fid);
% fid = fopen('hair_corrSpearman.dat','wt');
% fprintf(fid,'%5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e\n',rho1');
% fclose(fid);
% fid = fopen('hair_corrSpearmanSig.dat','wt');
% fprintf(fid,'%5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e %5e\n',p1');
% fclose(fid);

% plot correlation matrix incl. significance
% colormap: from blue - white - red
h4=[0 0 1
    0.1 0.1 1
    0.2 0.2 1
    0.3 0.3 1
    0.4 0.4 1
    0.5 0.5 1
    0.6 0.6 1
    0.7 0.7 1 
    0.8 0.8 1
    0.9 0.9 1
    1 0.9 0.9
    1 0.8 0.8 
    1 0.7 0.7 
    1 0.6 0.6
    1 0.5 0.5
    1 0.4 0.4
    1 0.3 0.3
    1 0.2 0.2 
    1 0.1 0.1    
    1 0 0];
% plot correlation coefficients
figure
[l m]=size(dat2);
x=0:m;
y=x;
rhoplot=rho;
rhoplot(:,m+1)=0;
rhoplot(m+1,:)=0;
pcolor(x,y,rhoplot)
colormap(h4)
colorbar
caxis([-1 1])
% xlabel('Variable number','FontWeight','bold')
% ylabel('Variable number','FontWeight','bold')
tit=sprintf('%s','Spearman correlation coefficient');
title(tit,'FontWeight','bold')
% plot circle (size: 10) on cells with significant correlation (p<0.05; correlation is significantly different from zero
xv=1:m;
yv=1:m;
[xx,yy] = meshgrid(xv,yv);
xxv=reshape(xx,m*m,1);
yyv=reshape(yy,m*m,1);
pv=reshape(p,m*m,1);
ii=find(pv<=0.05);
%pv(ii)=10;
hold on
scatter(xxv(ii)-0.5,yyv(ii)-0.5,10,'filled')
print(gcf,'-djpeg','hair_corrSpearman.jpg')



%% compare single influence for discrete values

% non-parametric one-way ANOVA to test the null hypothesis that independent
% samples from two or more groups come from distributions with equal medians,
% The data are assumed to come from continuous distributions that are 
% identical except possibly for location shifts due to group effects, but 
% are otherwise arbitrary.

% Puperty status
P = kruskalwallis(dat2(:,1),dat2(:,4))
% gender
P = kruskalwallis(dat2(:,1),dat2(:,6))
% Schulabschluss
P = kruskalwallis(dat2(:,1),dat2(:,8))
% Einzelkind
P = kruskalwallis(dat2(:,1),dat2(:,10))
% Einzelkind
P = kruskalwallis(dat2(:,1),dat2(:,12))


% %% regression 
% 
% y=dat2(:,1);
% x=dat2(:,2);
% figure
% plot(x,y,'*')
% glm=fitglm(x,y)



%% Multivariate, non-parametric regression model 
 
% build regression tree
% minimum number of values in leaves=25 !!!!!!!
RT=classregtree(dat2(:,2:end),dat2(:,1),'categorical',[3,5,9,11],'names',...
    {'BP_DIA','BP_SYS','PUB_STAT','AGE','GENDER','SOZIO_FAM','V_SCHULAB',...	
    'M_SCHULAB','EINZELKIND','EINKOMMEN','WI_FAS','BMI_ADJ','HEIGHT_ADJ',...
    'WEIGHT_ADJ'},'minleaf',25);
view (RT)

% feature importance
imp = varimportance(RT)

% Find the minimum-cost tree under condition: no of node >= 5 !!!!!!!!!!
mino=5;
[c,s,n,best] = test(RT,'cross',dat2(:,2:end),dat2(:,1))
if n(best+1) <= mino
    kk=find(n==mino)
    best=kk-1
end
RTmin = prune(RT,'level',best);
view(RTmin)
     