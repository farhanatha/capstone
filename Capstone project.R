## ===========  block 1: Set Directory  ========================================

setwd ("C:/Users/Farhan Atha/Google Drive/Atha/1-Career/036-Tetris Program DQLab/7-Capstone Project")

## ===========  block 2: Data dan Hipotesis  ===================================

# Load berita yang akan dijadikan 
library("readxl")
# xls files
hipotesis <- read_excel("data_teks.xls")
# Melihat isi tabel
View(hipotesis)

# Berdasarkan tabel "hipotesis", maka project ini akan menguji
# Ho: Tidak Terjadi disparitas (perbedaan) pengguna internet antara pulau jawa dan luar jawa
# H1: Terjadi disparitas pengguna internet antara pulau jawa dan luar jawa

## ===========  block 3: Hypothesis Testing  ===================================

# Sumber data: Susenas 2020
library("haven")
testing <- read_dta("cleaned_data.dta")
# Convert ke dalam kategorikal
testing$mobile <- factor(testing$mobile)
testing$island <- factor(testing$island)
head(testing)
summary(testing)

# Hypothesis testing menggunakan probit model, 
# karena bertujuan menganalisis pengaruh tinggal di pulau jawa/luar jawa
# dan mengetahui perbedaan probabilitas akses internet antara pulau jawa dan luar jawa.
probit_result <- glm(mobile ~ island, family = binomial(link = "probit"), data = testing)
summary (probit_result)

# Hasil regresi probit dari perbedaan probabilitas antara penduduk yang tinggal di Jawa dengan penduduk yang tinggal di luar Jawa.
# Ho: Tidak Terjadi disparitas pengguna internet antara pulau jawa dan luar jawa.
# H1: Terjadi disparitas pengguna internet antara pulau jawa dan luar jawa.

# Hasil: Karena nilainya signifikan, maka hasilnya menolak Ho dan menerima H1 karena nilainya kurang dari significant level (0,001).
# Kesimpulannya Terjadi disparitas pengguna internet antara pulau jawa dan luar jawa, 
# probabilitas akses internet di pulau jawa lebih tinggi 34% dibandingkan luar jawa.

## ===========  block 3: Visualisasi  ==========================================

## Membuat Visualisasi Persebaran akses Internet setiap provinsi
library("readxl")
library ("scales")
internet_by_provinces <- read_excel("internet.xlsx")
internet_by_provinces$Percentage <- (internet_by_provinces$Yes/internet_by_provinces$Total)
# Import data .shp file
library("rgdal")
library("dplyr")
library("sf")
library("tigris")
library("ggplot2")
# Import .shp data
map <-"shape/idn_admbnda_adm1_bps_20200401.shp"
glimpse(map)
map2 <- st_read(map)
glimpse(map2)
# Merge Data
merged_data <- geo_join(spatial_data=map2, 
                        data_frame=internet_by_provinces, by_sp="ADM1_EN", 
                        by_df="Provinces", how = "inner")
# Atur warna
mycol <- c("#bfdff6", "#53859e", "#2a607e")
# Menampilkan Plot Peta
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

pDATA<-ggplot()+
  geom_sf(data = merged_data,aes(fill = Percentage))+
  scale_fill_gradientn(colours = mycol)+
  theme(legend.title = element_text(size=10), 
        legend.text = element_text(size=9)) +
  theme_bw() +
  ditch_the_axes
pDATA

## Visualisasi rata-rata biaya internet by province
internet_cost <- read_dta("biaya internet.dta")

cost_province <- aggregate(x = internet_cost$sebulan,    # Specify data column
          by = list(internet_cost$prov),        # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# Rename variable province
merged_data1 <- geo_join(spatial_data=map2, 
                        data_frame=cost_province, by_sp="ADM1_EN", 
                        by_df="Group.1", how = "inner")

costDATA<-ggplot()+
  geom_sf(data = merged_data1,aes(fill = x))+
  scale_fill_gradientn(colours = mycol)+
  theme(legend.title = element_text(size=10), 
        legend.text = element_text(size=9)) +
  theme_bw() +
  ditch_the_axes
costDATA

## ===========  END OF R SCRIPT  ===============================================
