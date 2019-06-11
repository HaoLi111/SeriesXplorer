library(SeriesXplorerZoo)
x =SeriesXplorerZoo::Batrachion_Conway1988(10000)/1:10000

library(psd)

x_spc = pspectrum(x,plot=T)



x_est = pilot_spec(x)
plot(x_est)
str(x_est)

png(width =1280,height=720)
x_est = psd::pspectrum(x_ts,plot = T)
dev.off()

str(x_est$spec)
