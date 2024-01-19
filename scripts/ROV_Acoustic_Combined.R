

ROVtestdataframe<-as.data.frame(read.csv("wdata/siteinfo.csv", header = TRUE))
Acoustictestdata<-as.data.frame(read.csv("odata/Example100mDataframetest.csv", header=TRUE))

ROVacoustic<-left_join(ROVtestdataframe,Acoustictestdata, by = "Site_ID")

write.csv(ROVacoustic,"wdata/ROVacoustic_testdf.csv", row.names = FALSE)

testplot<-ggplot(ROVacoustic, aes(x=Abundance, y=NASC_15))+
  geom_point()
print(testplot)
