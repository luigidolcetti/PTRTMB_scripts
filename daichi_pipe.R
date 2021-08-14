library(RUNIMC)

memory.limit(128000)

MyStudy<-initStudy(fn_studyName = 'daichi_28_ROI',
                   fn_rootFolder = "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI",
                   fn_rawDataFolder = "C:/Users/k1343421/Documents/IMC/RAW_DATA_DAICHI_rename",
                   fn_whichColumns = 'named',
                   fn_transpose = F,
                   fn_overWrite = F,
                   fn_verbose = F)

st_samples(MyStudy)<-c(rep("ID50020007",3),
                       rep("ID40020013",12),
                       rep("ID70020001",5),
                       rep("ID60050001",8))
st_replicates(MyStudy)<-c(rep("REP01",28))
st_rois(MyStudy)<-c('ROI_001','ROI_002','ROI_003',
                    'ROI_001','ROI_002','ROI_003','ROI_004','ROI_005','ROI_006','ROI_007','ROI_008','ROI_009','ROI_010','ROI_011','ROI_012',
                    'ROI_001','ROI_002','ROI_003','ROI_004','ROI_010',
                    'ROI_001','ROI_002','ROI_015','ROI_016','ROI_017','ROI_018','ROI_019','ROI_020')
st_bioGroups(MyStudy)<-c(rep("HIGH_MDSC",15),rep("LOW_MDSC",13))
updateMetadata(MyStudy,saveToDisk = T)

newAnalysis(MyStudy,analysisName = 'FIRST_Analysis')

runIMC()

layersToUse<-ch_Rnames(MyStudy)


addFilter(MyStudy,'vanvliet',list(list(sigma=3,order=0,axis='x'),
                                  list(sigma=3,order=0,axis='y'),
                                  list(sigma=3,order=1,axis='x'),
                                  list(sigma=3,order=1,axis='y'),
                                  list(sigma=3,order=2,axis='x'),
                                  list(sigma=3,order=2,axis='y'),
                                  list(sigma=3,order=3,axis='x'),
                                  list(sigma=3,order=3,axis='y')),channels = layersToUse,append=F)

addFilter(MyStudy,'vanvliet',list(list(sigma=7,order=0,axis='x'),
                                  list(sigma=7,order=0,axis='y'),
                                  list(sigma=7,order=1,axis='x'),
                                  list(sigma=7,order=1,axis='y'),
                                  list(sigma=7,order=2,axis='x'),
                                  list(sigma=7,order=2,axis='y'),
                                  list(sigma=7,order=3,axis='x'),
                                  list(sigma=7,order=3,axis='y')),channels = layersToUse,append=T)

addFilter(MyStudy,'deriche',list(list(sigma=3,order=0,axis='x'),
                                 list(sigma=3,order=0,axis='y'),
                                 list(sigma=3,order=1,axis='x'),
                                 list(sigma=3,order=1,axis='y'),
                                 list(sigma=3,order=2,axis='x'),
                                 list(sigma=3,order=2,axis='y')),channels = layersToUse,append=T)

addFilter(MyStudy,'deriche',list(list(sigma=7,order=0,axis='x'),
                                 list(sigma=7,order=0,axis='y'),
                                 list(sigma=7,order=1,axis='x'),
                                 list(sigma=7,order=1,axis='y'),
                                 list(sigma=7,order=2,axis='x'),
                                 list(sigma=7,order=2,axis='y')),channels = layersToUse,append=T)

addFilter(MyStudy,'blur_anisotropic',list(list(amplitude=9),list(amplitude=3)),channels = layersToUse,append=T)

deployFilters(MyStudy,saveToDisk = T)

addExtractionDirectives(MyStudy,c(1,0.25),'core',append=F)

extractTrainingFeatures(MyStudy)

availableTF<-tf_featureList(MyStudy)

addClassificationDirectives(x = MyStudy,
                            method = 'randomForest',
                            methodParameters = list(responseVariable = 'label',
                                                    predictiveFeatures = availableTF,
                                                    PvalueTreshold = 0.3,
                                                    ntree=500,
                                                    mtry=35,
                                                    importance=F,
                                                    nodesize=1,
                                                    do.trace=10))

makeClassificationModel(x=MyStudy,method = 'randomForest')

archive(MyStudy)

classify(MyStudy,saveToDisk = T,method = 'randomForest')

archive(MyStudy)

for (i in 1:28){
  ggp<-plot.raster(MyStudy$currentAnalysis$classification[[i]],
                   fn_layer = 'label',
                   fn_colorsDiscrete = c('cyan',
                                         'green',
                                         'red',
                                         'gray',
                                         'yellow',
                                         'magenta4',
                                         'purple4',
                                         'black','black'))
  flnm<-file.path("C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test",
                  paste0(i,'_',st_samples(MyStudy)[i],'_',st_rois(MyStudy)[i],'.tiff'))
  ggplot2::ggsave(flnm,ggp[[1]],device = 'tiff',width = 6,height = 6,dpi = 300,compression='lzw')
}


addClassificationDirectives(x = MyStudy,
                            method = 'randomOnions',
                            methodParameters = list(responseVariable = 'SLI',
                                                    classificationLyr = 'label',
                                                    predictiveFeatures = availableTF,
                                                    prefix = 'sko_',
                                                    labels = tf_labelList(MyStudy)[1:7],
                                                    importance = F,
                                                    ntree = 250,
                                                    do.trace=10))

makeClassificationModel(x=MyStudy,method = 'randomOnions')

archive(MyStudy)

classify(MyStudy,saveToDisk = T,method = 'randomOnions')

archive(MyStudy)

attributes(MyStudy$currentAnalysis$classifier)

attr(MyStudy$currentAnalysis$classifier,'artnTimeStmp')<-NA

attr(MyStudy$currentAnalysis,'artnTimeStmp')<-NA

attr(MyStudy,'artnTimeStmp')<-NA

archive(MyStudy)

MyStudy<-retrieve("C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/study.xml"  )

addSegmentationDirectives(MyStudy,method = 'alligatorMap')

MyStudy$currentAnalysis$segmentationDirectives

MyStudy$currentAnalysis$segmentationDirectives@methodParameters$spikes<-12
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$radiusExpansion<-1.3
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$coverage<-0.25
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$areaAdaptRate<-0.33
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$roundnessAdaptRate<-0.33
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$targetArea<-'predicted_mean'
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$returnKinetic<-F
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$returnRasters<-F
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$cycleWindow<-20
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$seedOutScore<-4
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$favourForeing<-F
MyStudy$currentAnalysis$segmentationDirectives@methodParameters$fusion<-F

segment(MyStudy,
        labelLayer = names(MyStudy$currentAnalysis$classification[[1]])[c(10:14,16)])

archive(MyStudy)

distilExpression(MyStudy)

archive(MyStudy)

################## map plot ###################
ggp<-ggp.raster(fn_raster = MyStudy$raster[[7]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.95),
                fn_limitsType = 'quantile')
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[7],]
ggp<-ggp.geometry(fn_gg = ggp,
             fn_geometry = TEMP.geo,
             fn_borderCol = c('cyan','green2','red1','orange','purple','cornsilk4'),
             fn_borderVar = 'label')
ggp<-ggp+ggplot2::theme_void()

ggp

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  '7_ID4_ROI04.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)


ggp<-ggp.raster(fn_raster = MyStudy$raster[[1]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.95),
                fn_limitsType = 'quantile',
                fn_zoom = c(50,550,0,500))
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[1],]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','cornsilk4'),
                  fn_borderVar = 'label',
                  fn_zoom = c(50,550,0,500))
ggp<-ggp+ggplot2::theme_void()

ggp

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  '1_ID5_ROI01.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)

ggp<-ggp.raster(fn_raster = MyStudy$raster[[24]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.95),
                fn_limitsType = 'quantile',
                fn_zoom = c(250,750,0,500))
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[24],]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','cornsilk4'),
                  fn_borderVar = 'label',
                  fn_zoom = c(250,750,0,500))
ggp<-ggp+ggplot2::theme_void()

ggp

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  '24_ID6_ROI16.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)

ggp<-ggp.raster(fn_raster = MyStudy$raster[[18]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.95),
                fn_limitsType = 'quantile')
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[18],]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','cornsilk4'),
                  fn_borderVar = 'label')
ggp<-ggp+ggplot2::theme_void()

ggp

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  '18_ID7_ROI03.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)


#################some statistics#################
total.area<-aggregate(area~uid,MyStudy$currentAnalysis$exprs,sum)

partial.area<-aggregate(area~uid+label,MyStudy$currentAnalysis$exprs,sum)

normalised.area<-partial.area
for (i in 1:nrow(partial.area)){
  area.x<-partial.area[i,'area']
  uid.x<-partial.area[i,'uid']
  area.new<-area.x/total.area[total.area$uid==uid.x,'area']*100
  normalised.area[i,'area']<-area.new
}
normalised.area<-normalised.area[order(normalised.area[,'uid'],
                                       normalised.area[,'label']),]

normalised.area<-dplyr::left_join(normalised.area,MyStudy$studyTable[,c('uid','sample','ROI')],by='uid')

normalised.area<-normalised.area[order(
                                       normalised.area[,'ROI'],
                                       normalised.area[,'sample'],
                                       normalised.area[,'label']),]

ggplot2::ggplot(normalised.area)+
  ggplot2::geom_boxplot(ggplot2::aes(y=sample,x=area,fill=label))


#################### flowsom#######################


TEMP_exprs<-MyStudy$currentAnalysis$exprs
TEMP_exprs<-sf::st_drop_geometry(TEMP_exprs)
TEMP_exprs<-TEMP_exprs[,8:37]
TEMP_FCS<-flowCore::flowFrame(as.matrix(TEMP_exprs))
TEMP_transform<-scales::modulus_trans(0)
TEMP_som<-FlowSOM::FlowSOM(TEMP_FCS,
                           compensate = F,
                           transform = T,
                           transformFunction = TEMP_transform$transform,
                           toTransform = colnames(TEMP_exprs),
                           scale = T,
                           silent = F,
                           colsToUse = colnames(TEMP_exprs)[-c(2,3,7,16,29,30)],
                           seed = 123,
                           maxMeta = 4,
                           xdim=8,
                           ydim=8
)

saveRDS(TEMP_som,
        "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/FLOWSOM.R")  
FS_clusters<-FlowSOM::GetClusters(TEMP_som)

TEMP_exprs_flowSOM<-MyStudy$currentAnalysis$exprs
TEMP_exprs_flowSOM<-dplyr::bind_cols(TEMP_exprs_flowSOM,data.frame(FLOWSOM=FS_clusters))

# 
# TEMP_long<-TEMP_exprs_flowSOM
# TEMP_long<-sf::st_drop_geometry(TEMP_long)
# TEMP_long<-TEMP_long[,8:38]
# TEMP_long<-tidyr::pivot_longer(TEMP_long,names_to = 'Marker',values_to='exprs',cols=1:30)
# TEMP_long$Marker<-gsub("^.+?\\.(.+?)\\..*$", "\\1", TEMP_long$Marker)
# TEMP_long$FLOWSOM<-factor(TEMP_long$FLOWSOM)
# 

TEMP_exprs_flowSOM_matrix<-sf::st_drop_geometry(TEMP_exprs_flowSOM)
cN<-colnames(TEMP_exprs_flowSOM_matrix)

TEMP_exprs_flowSOM_matrix[,8:37]<-TEMP_transform$transform(TEMP_exprs_flowSOM_matrix[,8:37])

MFI_matrix<-aggregate(TEMP_exprs_flowSOM_matrix[,c(8:37)],TEMP_exprs_flowSOM_matrix[,c(3,38)],median)

umap.conffig.custom<-umap::umap.defaults

umap.conffig.custom$verbose<-T
umap.conffig.custom$n_epochs<-3000
umap.conffig.custom$spread<-5
umap.conffig.custom$min_dist<-2
# umap.conffig.custom$a<-0.6
# umap.conffig.custom$b<-0.6
ump<-umap::umap(MFI_matrix[,c(3:32)],config = umap.conffig.custom)

plot(ump$layout)


ump_map_layout<-cbind.data.frame(MFI_matrix[,c('label','FLOWSOM')],ump$layout)

TEMP_exprs_flowSOM_matrix_join<-dplyr::left_join(TEMP_exprs_flowSOM_matrix,MyStudy$studyTable[,c('uid','sample','ROI')],by='uid')

area_total_matrix<-aggregate(area~sample,TEMP_exprs_flowSOM_matrix_join,sum)
area_partial_matrix<-aggregate(area~sample+label+FLOWSOM,TEMP_exprs_flowSOM_matrix_join,sum)
for (i in unique(area_total_matrix$sample)){
  area_partial_matrix$area[area_partial_matrix$sample==i]<-area_partial_matrix$area[area_partial_matrix$sample==i]/area_total_matrix$area[area_total_matrix$sample==i]*1000
  # area_partial_matrix$area[area_partial_matrix$sample==i]<-compositions::clr(area_partial_matrix$area[area_partial_matrix$sample==i])
}


area_partial_matrix<-dplyr::left_join(area_partial_matrix,ump_map_layout,by=c('label','FLOWSOM'))
colnames(area_partial_matrix)[c(5,6)]<-c('uMAP-1','uMAP-2')
area_partial_matrix$FLOWSOM<-factor(as.character(area_partial_matrix$FLOWSOM))

ggplot2::ggplot(area_partial_matrix,ggplot2::aes(x=`uMAP-1`,y=`uMAP-2`,label=label))+
  # ggplot2::geom_label(ggplot2::aes(label=FLOWSOM,size=area,color=label),alpha=0.5)+
  ggplot2::geom_point(ggplot2::aes(size=area,color=label),alpha=0.5)+
  # ggrepel::geom_label_repel()+
  ggplot2::facet_wrap(facets = ggplot2::vars(sample))




cN<-colnames(TEMP_exprs_flowSOM)


nameIT(fn_exprs = TEMP_exprs_flowSOM,
       fn_Xclass = cN[38],
       fn_Yclass = cN[3],
       fn_dimToPlot = cN[8:37],
       fn_Newclass = 'newclass',
       fn_transform = TEMP_transform,
       fn_additionalCoords = NULL)
############################# quantile area ###################



qt<-aggregate(area~label+FLOWSOM,TEMP_exprs_flowSOM,quantile)
write.csv(qt,
          "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/quantiles.csv"  )

aggregate(area~label,TEMP_exprs_flowSOM,quantile)
########################### class heatmap ###############


TEMP_exprs_flowSOM_matrix<-sf::st_drop_geometry(TEMP_exprs_flowSOM)
TEMP_exprs_flowSOM_matrix_join<-dplyr::left_join(TEMP_exprs_flowSOM_matrix,MyStudy$studyTable[,c('uid','sample','ROI')],by='uid')
area_partial_matrix_consensus<-aggregate(area~label+FLOWSOM,TEMP_exprs_flowSOM_matrix_join,sum)


  for (fs in unique(area_partial_matrix_consensus$FLOWSOM)){
    tot_area<-sum(area_partial_matrix_consensus$area[area_partial_matrix_consensus$FLOWSOM==fs])
    area_partial_matrix_consensus$area[area_partial_matrix_consensus$FLOWSOM==fs]<-
      area_partial_matrix_consensus$area[area_partial_matrix_consensus$FLOWSOM==fs]/tot_area
  }

matrix_consensus<-tidyr::pivot_wider(area_partial_matrix_consensus,
                                     names_from = FLOWSOM,
                                     values_from = area)
matrix_consensus[is.na(matrix_consensus)]<-0
matrix_consensus_mat<-as.matrix(matrix_consensus[,-1])
colnames(matrix_consensus_mat)<-colnames(matrix_consensus[,-1])
rownames(matrix_consensus_mat)<-unlist(matrix_consensus[,1])

Rowv <- as.dendrogram(hclust(dist(t(matrix_consensus_mat))))
rowInd <- order.dendrogram(Rowv)
names(rowInd)<-colnames(matrix_consensus_mat)

area_partial_matrix_hm<-area_partial_matrix

area_partial_matrix_hm$FLOWSOM<-factor(area_partial_matrix_hm$FLOWSOM,levels = rowInd)
area_partial_matrix_hm$label<-factor(area_partial_matrix_hm$label)

for (smp in unique(area_partial_matrix_hm$sample)){
  for (fs in unique(area_partial_matrix_hm$FLOWSOM)){
    tot_area<-sum(area_partial_matrix_hm$area[area_partial_matrix_hm$sample==smp &
                                                area_partial_matrix_hm$FLOWSOM==fs])
    area_partial_matrix_hm$area[area_partial_matrix_hm$sample==smp &
                                  area_partial_matrix_hm$FLOWSOM==fs]<-
      area_partial_matrix_hm$area[area_partial_matrix_hm$sample==smp & area_partial_matrix_hm$FLOWSOM==fs]/tot_area
  }
}


ggp<-ggplot2::ggplot(area_partial_matrix_hm,ggplot2::aes(x=FLOWSOM,y=label))+
  ggplot2::geom_tile(ggplot2::aes(fill=area),col='gray30')+
  ggplot2::scale_fill_gradient2(low = 'blue',mid = 'black',high = 'red',midpoint = 0.5)+
  ggplot2::labs(x='cell level classification',y='pixel level classification')+
  ggplot2::facet_wrap(facets = ggplot2::vars(sample))+
    ggplot2::theme_dark()+ggplot2::theme(axis.text.x = ggplot2::element_text(size=6))

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  'heatmap.eps'),
  device = 'eps',
  width = 20,
  height = 3,
  units = 'in',
  dpi = 300)

area_partial_matrix_hm<-area_partial_matrix

area_partial_matrix_hm$FLOWSOM<-factor(area_partial_matrix_hm$FLOWSOM,levels = rowInd)
area_partial_matrix_hm$label<-factor(area_partial_matrix_hm$label)

for (smp in unique(area_partial_matrix_hm$sample)){
  for (fs in unique(area_partial_matrix_hm$label)){
    tot_area<-sum(area_partial_matrix_hm$area[area_partial_matrix_hm$sample==smp &
                                                area_partial_matrix_hm$label==fs])
    area_partial_matrix_hm$area[area_partial_matrix_hm$sample==smp &
                                  area_partial_matrix_hm$label==fs]<-
      area_partial_matrix_hm$area[area_partial_matrix_hm$sample==smp & area_partial_matrix_hm$label==fs]/tot_area
  }
}


ggplot2::ggplot(area_partial_matrix_hm,ggplot2::aes(x=FLOWSOM,y=label))+
  ggplot2::geom_tile(ggplot2::aes(fill=area),col='gray30')+
  ggplot2::scale_fill_gradient2(low = 'blue',mid = 'black',high = 'red',midpoint = 0.2)+
  ggplot2::facet_wrap(facets = ggplot2::vars(sample))+
  ggplot2::theme_dark()



###################### stat by groups ###################

###################### stat by groups cd8###################

newClassification<-dplyr::left_join(TEMP_exprs_flowSOM,
                                    MyStudy$studyTable[,c('uid','sample','ROI')],
                                    by='uid')

newClassification<-sf::st_drop_geometry(newClassification)
newClassification<-cbind.data.frame(newClassification,newClass = 'other',stringsAsFactors=F)

newc.total.area<-aggregate(area~sample+ROI,newClassification,sum)



newc<-newClassification

newc$newClass[newc$label == 'sko_CD8' & newc$FLOWSOM == 33]<-'CD8_33'
newc$newClass[newc$label == 'sko_CD8' & newc$FLOWSOM == 41]<-'CD8_41'
newc$newClass[newc$label == 'sko_CD8' & newc$FLOWSOM == 42]<-'CD8_42'
newc<-newc[newc$label == 'sko_CD8',]

newc<-aggregate(area~sample+ROI+newClass,newc,sum)

for (i in 1:nrow(newc.total.area)){
  smp<-newc.total.area[i,'sample']
  roi<-newc.total.area[i,'ROI']
  area.tot<-newc.total.area[i,'area']
  newc$area[newc$sample==smp & newc$ROI==roi]<-newc$area[newc$sample==smp & newc$ROI==roi]/area.tot*10^6
}

ggp<-ggplot2::ggplot(newc,ggplot2::aes(y=area,x=sample))+
  ggplot2::geom_boxplot()+
  ggplot2::geom_jitter(position=ggplot2::position_jitter(0.2))+
  ggplot2::scale_x_discrete(label=abbreviate)+
  ggplot2::labs(y=expression('normalized area'~ µm^2/mm^2))+
  ggplot2::facet_wrap(facets = ggplot2::vars(newClass),scales = 'free_y')+
  ggplot2::theme_classic()

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  'BOXPLOT_CD8.eps'),
  device = 'eps',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)

kruskal.test(area~sample,newc[newc$newClass=='other',])
pairwise.wilcox.test(newc$area[newc$newClass=='other'], newc$sample[newc$newClass=='other'],
                     p.adjust.method = "BH")


###################### stat by groups cd4###################

newc<-newClassification

for (i in c(34,49,28,50,58,51,57)){
newc$newClass[newc$label == 'sko_CD4' & newc$FLOWSOM == i]<-paste0('CD4_',i)
}

newc<-newc[newc$label == 'sko_CD4',]

newc<-aggregate(area~sample+ROI+newClass,newc,sum)

for (i in 1:nrow(newc.total.area)){
  smp<-newc.total.area[i,'sample']
  roi<-newc.total.area[i,'ROI']
  area.tot<-newc.total.area[i,'area']
  newc$area[newc$sample==smp & newc$ROI==roi]<-newc$area[newc$sample==smp & newc$ROI==roi]/area.tot*10^6
}

ggp<-ggplot2::ggplot(newc,ggplot2::aes(y=area,x=sample))+
  ggplot2::geom_boxplot()+
  ggplot2::geom_jitter(position=ggplot2::position_jitter(0.2))+
  ggplot2::scale_x_discrete(label=abbreviate)+
  ggplot2::labs(y=expression('normalized area'~ µm^2/mm^2))+
  ggplot2::facet_wrap(facets = ggplot2::vars(newClass),scales = 'free_y')+
  ggplot2::theme_classic()

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  'BOXPLOT_CD4.eps'),
  device = 'eps',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)

kruskal.test(area~sample,newc[newc$newClass=='CD4_58',])
pairwise.wilcox.test(newc$area[newc$newClass=='CD4_58'], newc$sample[newc$newClass=='CD4_58'],
                     p.adjust.method = "BH")


###################### stat by groups MLYD###################

newc<-newClassification

for (i in c(60,61,9,64,26,37,48)){
  newc$newClass[newc$label == 'sko_MLYD' & newc$FLOWSOM == i]<-paste0('MLYD_',i)
}

newc<-newc[newc$label == 'sko_MLYD',]

newc<-aggregate(area~sample+ROI+newClass,newc,sum)

for (i in 1:nrow(newc.total.area)){
  smp<-newc.total.area[i,'sample']
  roi<-newc.total.area[i,'ROI']
  area.tot<-newc.total.area[i,'area']
  newc$area[newc$sample==smp & newc$ROI==roi]<-newc$area[newc$sample==smp & newc$ROI==roi]/area.tot*10^6
}

ggp<-ggplot2::ggplot(newc,ggplot2::aes(y=area,x=sample))+
  ggplot2::geom_boxplot()+
  ggplot2::geom_jitter(position=ggplot2::position_jitter(0.2))+
  ggplot2::scale_x_discrete(label=abbreviate)+
  ggplot2::labs(y=expression('normalized area'~ µm^2/mm^2))+
  ggplot2::facet_wrap(facets = ggplot2::vars(newClass),scales = 'free_y')+
  ggplot2::theme_classic()

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  'BOXPLOT_MLYD.eps'),
  device = 'eps',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)


kruskal.test(area~sample,newc[newc$newClass=='MLYD_9',])
pairwise.wilcox.test(newc$area[newc$newClass=='MLYD_9'], newc$sample[newc$newClass=='MLYD_9'],
                     p.adjust.method = "BH")

###################### stat by groups CD20###################

newc<-newClassification

for (i in c(59)){
  newc$newClass[newc$label == 'sko_CD20' & newc$FLOWSOM == i]<-paste0('CD20_',i)
}

newc<-newc[newc$label == 'sko_CD20',]

newc<-aggregate(area~sample+ROI+newClass,newc,sum)

for (i in 1:nrow(newc.total.area)){
  smp<-newc.total.area[i,'sample']
  roi<-newc.total.area[i,'ROI']
  area.tot<-newc.total.area[i,'area']
  newc$area[newc$sample==smp & newc$ROI==roi]<-newc$area[newc$sample==smp & newc$ROI==roi]/area.tot*10^6
}

ggp<-ggplot2::ggplot(newc,ggplot2::aes(y=area,x=sample))+
  ggplot2::geom_boxplot()+
  ggplot2::geom_jitter(position=ggplot2::position_jitter(0.2))+
  ggplot2::scale_x_discrete(label=abbreviate)+
  ggplot2::labs(y=expression('normalized area'~ µm^2/mm^2))+
  ggplot2::facet_wrap(facets = ggplot2::vars(newClass),scales = 'free_y')+
  ggplot2::theme_classic()

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  'BOXPLOT_CD20.eps'),
  device = 'eps',
 
  width = 10,
  height = 5,
  units = 'in',
  dpi = 300)


kruskal.test(area~sample,newc[newc$newClass=='other',])
pairwise.wilcox.test(newc$area[newc$newClass=='other'], newc$sample[newc$newClass=='other'],
                     p.adjust.method = "BH")



######################## inspect CD4-28 #########


polygon_subset<-TEMP_exprs_flowSOM

polygon_subset<-polygon_subset[polygon_subset$label=='sko_CD4' & polygon_subset$FLOWSOM=='28',]
nrow(polygon_subset)
hist(polygon_subset$area)

ggp<-ggp.raster(fn_raster = MyStudy$raster[[1]]$x154sm.cd11c.sm154di.,
                fn_limits = c(0,0.95),
                fn_limitsType = 'quantile',
                fn_zoom = c(100,300,100,300))
TEMP.geo<-polygon_subset[polygon_subset$uid==st_uids(MyStudy)[1],]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','cornsilk4'),
                  fn_borderVar = 'label',
                  fn_zoom = c(100,300,100,300))

ggp




######################## final submission#########


library(RUNIMC)
memory.limit(32000)
MyStudy<-retrieve(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/study.xml"  
)

TEMP_som<-readRDS("C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/FLOWSOM.R")

FS_clusters<-FlowSOM::GetClusters(TEMP_som)
TEMP_exprs_flowSOM<-MyStudy$currentAnalysis$exprs
TEMP_exprs_flowSOM<-dplyr::bind_cols(TEMP_exprs_flowSOM,data.frame(FLOWSOM=FS_clusters))


newClassification<-dplyr::left_join(TEMP_exprs_flowSOM,
                                    MyStudy$studyTable[,c('uid','sample','ROI')],
                                    by='uid')

newClassification<-sf::st_drop_geometry(newClassification)
newClassification<-cbind.data.frame(newClassification,newClass = 'other',stringsAsFactors=F)



newc<-newClassification

for (i in c(37)){
  newc$newClass[newc$label == 'sko_MLYD' & newc$FLOWSOM == i]<-paste0('MLYD_',i)
}

newc<-newc[newc$label == 'sko_MLYD',]

newc<-aggregate(area~sample+ROI+newClass,newc,sum)
newc.total.area<-aggregate(area~sample+ROI,newClassification,sum)

for (i in 1:nrow(newc.total.area)){
  smp<-newc.total.area[i,'sample']
  roi<-newc.total.area[i,'ROI']
  area.tot<-newc.total.area[i,'area']
  newc$area[newc$sample==smp & newc$ROI==roi]<-newc$area[newc$sample==smp & newc$ROI==roi]/area.tot*10^6
}

ggp<-ggplot2::ggplot(newc,ggplot2::aes(y=area,x=sample))+
  ggplot2::geom_boxplot()+
  ggplot2::geom_jitter(position=ggplot2::position_jitter(0.2))+
  ggplot2::scale_x_discrete(label=abbreviate)+
  ggplot2::labs(y=expression('normalized area'~ µm^2/mm^2))+
  ggplot2::facet_wrap(facets = ggplot2::vars(newClass),scales = 'free_y')+
  ggplot2::theme_classic()

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps",
  'BOXPLOT_MLYD_37.eps'),
  device = 'eps',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)

######################## ..fingerprint#########

TEMP_transform<-scales::modulus_trans(0)


newc<-newClassification

newc$newClass<-paste0(newc$label,newc$FLOWSOM)

cn<-colnames(newc)[8:37]

for (i in cn){
  newc[,i]<-TEMP_transform$transform(newc[,i])
}

for (i in cn){
  
new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))

new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]

spt<-which(new_quart$newClass=='sko_MLYD37')

postscript(file = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint MLYD_37",
  paste0(i,'.eps')),
  width = 5.4,
  height = 6.3)

plot(new_quart[,2][,2],pch=NA,main=i)
lines(new_quart[,2][,2],col='black')
points(new_quart[,2][,3],col='red',pch=19,cex=0.5)
points(new_quart[,2][,1],col='blue',pch=19,cex=0.5)
points(spt,new_quart[,2][spt,2],col='red',pch=19)
# lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
# lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
abline(v=spt,lty=5)
abline(h=new_quart[,2][spt,2],lty=5)
dev.off()
}

######################## ..fingerprint#########

TEMP_transform<-scales::modulus_trans(0)


newc<-newClassification

newc$newClass<-paste0(newc$label,newc$FLOWSOM)

cn<-colnames(newc)[8:37]

for (i in cn){
  newc[,i]<-TEMP_transform$transform(newc[,i])
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_MLYD61')
  
  tiff(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint MLYD_61",
    paste0(i,'.tiff')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i)
  lines(new_quart[,2][,2],col='black')
  points(new_quart[,2][,3],col='red',pch=19,cex=0.5)
  points(new_quart[,2][,1],col='blue',pch=19,cex=0.5)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}


######################## ..fingerprint#########

TEMP_transform<-scales::modulus_trans(0)


newc<-newClassification

newc$newClass<-paste0(newc$label,newc$FLOWSOM)

cn<-colnames(newc)[8:37]

for (i in cn){
  newc[,i]<-TEMP_transform$transform(newc[,i])
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_MLYD9')
  
  tiff(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint MLYD_9",
    paste0(i,'.tiff')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i)
  lines(new_quart[,2][,2],col='black')
  points(new_quart[,2][,3],col='red',pch=19,cex=0.5)
  points(new_quart[,2][,1],col='blue',pch=19,cex=0.5)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}

######################## ..fingerprint#########

TEMP_transform<-scales::modulus_trans(0)


newc<-newClassification

newc$newClass<-paste0(newc$label,newc$FLOWSOM)

cn<-colnames(newc)[8:37]

for (i in cn){
  newc[,i]<-TEMP_transform$transform(newc[,i])
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_MLYD60')
  
  tiff(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint MLYD_60",
    paste0(i,'.tiff')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i)
  lines(new_quart[,2][,2],col='black')
  points(new_quart[,2][,3],col='red',pch=19,cex=0.5)
  points(new_quart[,2][,1],col='blue',pch=19,cex=0.5)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}




################## map plot ###################
ggp<-ggp.raster(fn_raster = MyStudy$raster[[7]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.999),
                fn_limitsType = 'quantile',
                fn_zoom = c(0,250,150,400))
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[7],]
TEMP.geo<-TEMP.geo[TEMP.geo$area>15,]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','blue'),
                  fn_borderVar = 'label',
                  fn_zoom = c(0,250,150,400))
ggp<-ggp+ggplot2::theme_void()
ggp<-ggp+ggplot2::theme(legend.position = 'none')


ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/NEWMAP",
  '7_ID4_ROI04.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)


ggp<-ggp.raster(fn_raster = MyStudy$raster[[1]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.999),
                fn_limitsType = 'quantile',
                fn_zoom = c(350,600,0,250))
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[1],]
TEMP.geo<-TEMP.geo[TEMP.geo$area>15,]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','blue'),
                  fn_borderVar = 'label',
                  fn_zoom = c(350,600,0,250))
ggp<-ggp+ggplot2::theme_void()
ggp<-ggp+ggplot2::theme(legend.position = 'none')

ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/NEWMAP",
  '1_ID5_ROI01.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)

ggp<-ggp.raster(fn_raster = MyStudy$raster[[24]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.999),
                fn_limitsType = 'quantile',
                fn_zoom = c(400,650,0,250))
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[24],]
TEMP.geo<-TEMP.geo[TEMP.geo$area>15,]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','blue'),
                  fn_borderVar = 'label',
                  fn_zoom = c(400,650,0,250))
ggp<-ggp+ggplot2::theme_void()
ggp<-ggp+ggplot2::theme(legend.position = 'none')


ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/NEWMAP",
  '24_ID6_ROI16.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)

ggp<-ggp.raster(fn_raster = MyStudy$raster[[18]]$x191ir.dna1.ir191di.,
                fn_limits = c(0,0.999),
                fn_limitsType = 'quantile',
                fn_zoom = c(100,350,100,350))
TEMP.geo<-MyStudy$currentAnalysis$exprs[MyStudy$currentAnalysis$exprs$uid==st_uids(MyStudy)[18],]
TEMP.geo<-TEMP.geo[TEMP.geo$area>15,]
ggp<-ggp.geometry(fn_gg = ggp,
                  fn_geometry = TEMP.geo,
                  fn_borderCol = c('cyan','green2','red1','orange','purple','blue'),
                  fn_borderVar = 'label',
                  fn_zoom = c(100,350,100,350))
ggp<-ggp+ggplot2::theme_void()
ggp<-ggp+ggplot2::theme(legend.position = 'none')


ggplot2::ggsave(plot = ggp,filename = file.path(
  "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/NEWMAP",
  '18_ID7_ROI03.tiff'),
  device = 'tiff',
  compression='lzw',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300)



######################## ..fingerprint B26#########

TEMP_transform<-scales::modulus_trans(0)


newc<-newClassification

newc$newClass<-paste0(newc$label,newc$FLOWSOM)

cn<-colnames(newc)[8:37]

for (i in cn){
  newc[,i]<-TEMP_transform$transform(newc[,i])
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_MLYD26')
  
  jpeg(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint MLYD_26",
    paste0(i,'.jpg')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i,xlim=c(0,(length(new_quart[,2][,2])+1)))
  lines(new_quart[,2][,2],col='black')
  lines(new_quart[,2][,3],col='red',type='s',add=T)
  lines(new_quart[,2][,1],col='blue',type='s',border=NULL,add=T)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_MLYD37')
  
  jpeg(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint MLYD_37",
    paste0(i,'.jpg')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i,xlim=c(0,(length(new_quart[,2][,2])+1)))
  lines(new_quart[,2][,2],col='black')
  lines(new_quart[,2][,3],col='red',type='s',add=T)
  lines(new_quart[,2][,1],col='blue',type='s',border=NULL,add=T)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_CD428')
  
  jpeg(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint CD4_28",
    paste0(i,'.jpg')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i,xlim=c(0,(length(new_quart[,2][,2])+1)))
  lines(new_quart[,2][,2],col='black')
  lines(new_quart[,2][,3],col='red',type='s',add=T)
  lines(new_quart[,2][,1],col='blue',type='s',border=NULL,add=T)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_CD449')
  
  jpeg(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint CD4_49",
    paste0(i,'.jpg')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i,xlim=c(0,(length(new_quart[,2][,2])+1)))
  lines(new_quart[,2][,2],col='black')
  lines(new_quart[,2][,3],col='red',type='s',add=T)
  lines(new_quart[,2][,1],col='blue',type='s',border=NULL,add=T)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_CD457')
  
  jpeg(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint CD4_57",
    paste0(i,'.jpg')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i,xlim=c(0,(length(new_quart[,2][,2])+1)))
  lines(new_quart[,2][,2],col='black')
  lines(new_quart[,2][,3],col='red',type='s',add=T)
  lines(new_quart[,2][,1],col='blue',type='s',border=NULL,add=T)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}

for (i in cn){
  
  new_quart<-aggregate(newc[,i,drop=F],newc[,'newClass',drop=F],quantile,c(0.25,0.5,0.75))
  
  new_quart<-new_quart[order(new_quart[,2][,2],decreasing = F),]
  
  spt<-which(new_quart$newClass=='sko_CD458')
  
  jpeg(file = file.path(
    "C:/Users/k1343421/Documents/IMC/ROOT_TEST_RUN_DAICHI/Daichi_28_ROI/daichi_28_ROI/analysis/FIRST_Analysis/test/Segmented_maps/fingerprint CD4_58",
    paste0(i,'.jpg')),
    width = 200,
    height = 200)
  
  plot(new_quart[,2][,2],pch=NA,main=i,xlim=c(0,(length(new_quart[,2][,2])+1)))
  lines(new_quart[,2][,2],col='black')
  lines(new_quart[,2][,3],col='red',type='s',add=T)
  lines(new_quart[,2][,1],col='blue',type='s',border=NULL,add=T)
  points(spt,new_quart[,2][spt,2],col='red',pch=19)
  # lines(x=c(0,spt),y=c(new_quart[,2][spt,2],new_quart[,2][spt,2]),lty=5)
  # lines(x=c(spt,spt),y=c(0,new_quart[,2][spt,2]),lty=5)
  abline(v=spt,lty=5)
  abline(h=new_quart[,2][spt,2],lty=5)
  dev.off()
}
