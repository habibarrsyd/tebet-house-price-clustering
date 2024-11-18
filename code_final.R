# Load library
library(ggplot2)
library(factoextra)
library(dplyr)
library(openxlsx)
library(psych)

# Baca file Excel
datarumah <- read.xlsx("D:/SEMESTER 4/METKUAN/datarumah.xlsx", sheet = 1)
View(datarumah)

# Membagi nilai pada kolom 'HARGA' dengan 1.000.000 dan mengubahnya menjadi integer
datarumah <- datarumah %>%
  mutate(HARGA = as.integer(HARGA / 1000000))
View(datarumah)

# Misalkan 'population' adalah data populasi dari kolom 'HARGA'
population <- datarumah$HARGA
#cek p-value untuk sampel
set.seed(123)
sample<-sample(population,200)

# Uji t-test untuk membandingkan mean populasi dan sampel
t_test_result <- t.test(sample, mu = mean(population))
print(t_test_result)

# Mengambil sampel acak sebanyak 200 dari populasi
set.seed(123)  # Untuk reproducibility
sample_indices <- sample(1:nrow(datarumah), 200)

# Membuat data frame baru yang berisi 200 sampel
datapakai <- datarumah[sample_indices, ]
View(datapakai)


# Memindahkan kolom non-numeric
databaru <- datapakai[,-c(1, 2)]
View(databaru)
head(databaru)



summary(databaru)
str(databaru)

# Menentukan jumlah cluster menggunakan metode elbow
fviz_nbclust(databaru, kmeans, method = "wss")

# K-means clustering dengan 4 cluster
final <- kmeans(databaru, 4)
final

# Mengecek sum of squares
cat("Total sum of squares (totss):", final$totss, "\n")
cat("Within-cluster sum of squares (tot.withinss):", final$tot.withinss, "\n")
cat("Between-cluster sum of squares (betweenss):", final$betweenss, "\n")

# Visualisasi cluster
fviz_cluster(final, data = databaru)

# Membuat data frame akhir yang berisi data dan cluster
finalakhir <- data.frame(databaru, cluster = final$cluster)
View(finalakhir)

# Menghitung rata-rata untuk setiap cluster
cluster_summary <- finalakhir %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

print(cluster_summary)

# Menambahkan kolom 'NAMA RUMAH' ke data frame akhir
finalakhir <- cbind(NAMA.RUMAH = datapakai$NAMA.RUMAH, finalakhir)
View(finalakhir)
#View(finalakhir)
finalakhir <- finalakhir %>%
  arrange(cluster)
View(finalakhir)
head(finalakhir)
