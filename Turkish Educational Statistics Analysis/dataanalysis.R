#Install necessary packages
install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

# 1) Bölgelere göre cinsiyetlerin dağılımı,	bölge bazında yüzdeler, bar chart

#Calculating the percentages
bir <- veri %>%
  group_by(bolge, cinsiyet) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE)) %>%
  mutate(yuzde = total_frekans / sum(total_frekans) * 100) %>%
  ungroup()

#Plotting the bar chart
ggplot(bir, aes(x = bolge, y = yuzde, fill = cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bölgelere Göre Cinsiyetlerin Dağılımı", x = "Bölge", y = "Yüzde") +
  theme_minimal()



# 2) Bölgelere göre yaş gruplarının dağılımları, bölge bazında yüzdeler, bar chart

#Calculating the percentages
iki <- veri %>%
  group_by(bolge, yas_grubu) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE)) %>%
  mutate(yuzde = total_frekans / sum(total_frekans) * 100) %>%
  ungroup()

#Plotting the bar chart
ggplot(iki, aes(x = bolge, y = yuzde, fill = yas_grubu)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bölgelere Göre Yaş Gruplarının Dağılımı", x = "Bölge", y = "Yüzde") +
  theme_minimal()



# 3) Bölgelere göre farklı eğitim düzeylerinin dağılımı, bölge bazında yüzdeler, bar chart

#Calculating the percentages
uc <- veri %>%
  group_by(bolge, egitim_duzey) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE)) %>%
  mutate(yuzde = total_frekans / sum(total_frekans) * 100) %>%
  ungroup()

#Plotting the bar chart
ggplot(uc, aes(x = bolge, y = yuzde, fill = egitim_duzey)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bölgelere Göre Eğitim Düzeylerinin Dağılımı", x = "Bölge", y = "Yüzde") +
  theme_minimal()



# 4) Türkiye çapında cinsiyetlere göre yaş gruplarının dağılımları, cinsiyetlere göre yaş gruplarının yüzdeleri, bar chart (kadın ve erkek aynı plotta gruplanmış şekilde)

#Calculating the percentages
dort <- veri %>%
  group_by(cinsiyet, yas_grubu) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE)) %>%
  mutate(yuzde = total_frekans / sum(total_frekans) * 100) %>%
  ungroup()

#Plotting the bar chart
ggplot(dort, aes(x = yas_grubu, y = yuzde, fill = cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cinsiyetlere Göre Yaş Gruplarının Dağılımı", x = "Yaş Grubu", y = "Yüzde") +
  theme_minimal()



# 5) Türkiye çapında cinsiyetlere göre eğitim düzeylerinin dağılımları, cinsiyetlere göre eğitim düzeylerinin yüzdeleri, kadın ve erkek için iki ayrı pie chart

#Calculating the percentages
bes <- veri %>%
  group_by(cinsiyet, egitim_duzey) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE)) %>%
  mutate(yuzde = total_frekans / sum(total_frekans) * 100) %>%
  ungroup()

#Plotting the pie chart
ggplot(bes, aes(x = "", y = yuzde, fill = egitim_duzey)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~cinsiyet) +
  labs(title = "Cinsiyetlere Göre Eğitim Düzeylerinin Dağılımı", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())



# 6) Yıllara göre "okuma yazma bilmeyen" kadın ve erkek sayılarındaki değişimler, yıl bazında ilgili eğitim düzeyindeki ortalama kadın ve erkek sayıları, bar chart (kadın ve erkek aynı plotta gruplanmış şekilde)

#Calculating the percentages
alti <- veri %>%
  filter(egitim_duzey == "Okuma Yazma Bilmeyen") %>%
  group_by(yil, cinsiyet) %>%
  summarise(ortalama_sayi = mean(frekans, na.rm = TRUE)) %>%
  ungroup()

#Plotting the bar chart
ggplot(alti, aes(x = yil, y = ortalama_sayi, fill = cinsiyet)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yıllara Göre 'Okuma Yazma Bilmeyen' Kadın ve Erkek Sayıları", x = "Yıl", y = "Ortalama Sayı") +
  theme_minimal()



# 7) Türkiye çapında cinsiyetlere göre "okuma yazma bilen fakat bir okul bitirmeyen" nüfusun 2017'den 2021'e değişimleri,	cinsiyet bazında ilgili eğitim düzeyinin yıllara göre frekansları, line chart (kadın ve erkek aynı plotta)











# 8) Türkiye çapında doktoralılar içerisinde kadınların oranlarının yıllar içindeki değişimleri,	yıllara göre cinsiyet bazında yüzdeler, line chart

# "Doktora" eğitim durumunu filtreleme
df_filtered2 <- veri %>%
  filter(egitim_duzey == "Doktora")


# Kadın doktoralı oranlarını hesaplama
df_ratios2 <- df_filtered2 %>%
  group_by(yil, cinsiyet) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE)) %>%
  group_by(yil) %>%
  mutate(total_doktora = sum(total_frekans)) %>%
  ungroup() %>%
  filter(cinsiyet == "Kadın") %>%
  mutate(orani = total_frekans / total_doktora) %>%
  select(yil, oranı = orani)

print(df_ratios)

# Çizgi grafiği oluşturma
df_ratios2 %>%
  ggplot(aes(x = yil, y = oranı)) +
  geom_line(size = 1.2, color = "blue") +  # Çizgi rengi ve kalınlığı
  geom_point(size = 3, color = "blue") +   # Nokta boyutu ve rengi
  scale_y_continuous(labels = scales::percent) +  # Y eksenindeki oranları yüzdelik olarak gösterir
  labs(title = "Türkiye Çapında Doktoralılar İçerisinde Kadınların Oranlarının Yıllar İçindeki Değişimleri",
       x = "Yıl",
       y = "Kadın Doktoralı Oranı (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )







# 9) 2017'den 2021'e bölgelere göre erkek doktoralı oranlarındaki değişim,	bölgeler ve yıllara göre yüzdeler, line chart (tüm bölgeler tek bir plotta)

df_filtered <- veri %>%
  filter(cinsiyet == "Erkek" & egitim_duzey == "Doktora")
# Toplam frekanslar
df_totals <- veri %>%
  group_by(yil, bolge) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE))

# Erkek doktoralı oranları
df_ratios <- df_filtered %>%
  group_by(yil, bolge) %>%
  summarise(erkek_doktorali_frekans = sum(frekans, na.rm = TRUE)) %>%
  left_join(df_totals, by = c("yil", "bolge")) %>%
  mutate(erkek_doktorali_orani = erkek_doktorali_frekans / total_frekans * 100)

print(df_ratios)

# Çizgi grafiği oluşturma
df_ratios %>%
  ggplot(aes(x = yil, y = erkek_doktorali_orani, color = bolge, group = bolge)) +
  geom_line(size = 1.2) +  # Çizgi kalınlığı
  geom_point(size = 3) +   # Nokta boyutu
  scale_y_continuous(labels = scales::percent) +  # Y eksenindeki oranları yüzdelik olarak gösterir
  labs(title = "2017'den 2021'e Bölgeler Bazında Erkek Doktoralı Oranlarındaki Değişim",
       x = "Yıl",
       y = "Erkek Doktoralı Oranı (%)",
       color = "Bölge") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )






# 10) Türkiye çapında farklı eğitim düzeylerindeki kadınların sayısı ile erkeklerinki arasında istatistiksel olarak anlamlı bir faklılık var mıdır? İki cinsiyet altında farklı yaş ve eğitim düzeylerindeki popülasyonun frekansları, test sonuçları

# Eğitim durumu ve cinsiyete göre frekans tablosu oluşturma
df_egitim <- table(veri$egitim_duzey, veri$cinsiyet)

print(df_egitim)

# Ki-kare testi uygulama
test_result <- chisq.test(df_egitim)

# Beklenen frekansları görüntüleme
print(test_result$expected)

# Beklenen frekansların her bir hücrede 5'ten büyük olup olmadığını kontrol etme
any(test_result$expected < 5)

# Ki-kare testi sonuçlarını görüntüleme
print(test_result)

#p-değeri 0.05'ten büyük olduğu için (p = 1), hipotez testi sonuçlarına göre, kadınlar ve erkekler arasında eğitim düzeyleri açısından anlamlı bir fark olmadığını söyleyebiliriz.








# 11) Yalnızca eldeki 5 yıllık verilerden hareket ederek 2022 için Türkiye'de "Okuma Yazma Bilmeyen" nüfusun sayısını tahmin ediniz.	Test sonuçları ve line chart (2022 tahminimiz de plotta yer alacak)

# Gerekli paketleri yükleme ve çağırma
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forecast")
library(dplyr)
library(ggplot2)
library(forecast)


# "Okuma Yazma Bilmeyen" verilerini filtreleme ve yıllık toplamları hesaplama
onbir <- veri %>%
  filter(egitim_duzey == "Okuma Yazma Bilmeyen") %>%
  group_by(yil) %>%
  summarise(okuma_yazma_bilmeyen = sum(frekans, na.rm = TRUE)) %>%
  arrange(yil)

# Zaman serisi nesnesi oluşturma (en az bir gözlem olduğundan emin olalım)
if(nrow(onbir) > 0) {
  ts_data <- ts(onbir$okuma_yazma_bilmeyen, start = min(onbir$yil), frequency = 1)
  
  # ARIMA modeli ile tahmin yapma
  fit <- auto.arima(ts_data)
  forecasted <- forecast(fit, h = 1)  # 1 yıl ileriye tahmin
  
  # Tahmin edilen değeri görüntüleme
  print(forecasted)
  
  # Tahmin edilen değeri veri çerçevesine ekleme
  df_tahmin <- data.frame(
    yil = max(onbir$yil) + 1,
    okuma_yazma_bilmeyen = as.numeric(forecasted$mean)
  )
  
  print(df_tahmin)
  
  # Sütun adlarının ve türlerinin uyumlu olup olmadığını kontrol etme
  print(colnames(onbir))
  print(colnames(df_tahmin))
  print(sapply(onbir, class))
  print(sapply(df_tahmin, class))
  
  # Veri çerçevelerini birleştirme
  df_all <- rbind(onbir, df_tahmin)
} else {
  stop("Veri seti yeterli gözlem içermiyor.")
}

# Line chart çizme
ggplot(df_all, aes(x = yil, y = okuma_yazma_bilmeyen)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_vline(xintercept = max(onbir$yil) + 0.5, linetype = "dashed", color = "grey") +
  ggtitle("Türkiye'de Okuma Yazma Bilmeyen Nüfusun Tahmini (2017-2022)") +
  xlab("Yıl") +
  ylab("Okuma Yazma Bilmeyen Nüfus") +
  theme_minimal()





# 12) Genel eğitim göstergelerine dayanarak doğru distance metriği ve analiz yöntemi kullanarak bölgeleri kümeleyiniz.	Bölgelere göre tüm eğitim düzeylerindeki nüfusun gösterildiği kümeleme datası (sütunlarda eğitim düzeyleri olacak ve yaş ya da cinsiyete göre değil genel toplamlar ile ifade edilecek) Dendrogram

# Gerekli paketleri yükleme ve çağırma
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)

# Eğitim düzeylerine göre bölgelere ait toplam nüfusları hesaplama
oniki <- veri %>%
  group_by(bolge, egitim_duzey) %>%
  summarise(total_frekans = sum(frekans, na.rm = TRUE)) %>%
  pivot_wider(names_from = egitim_duzey, values_from = total_frekans, values_fill = 0)

# Sonuçları kontrol etme
print(oniki)








