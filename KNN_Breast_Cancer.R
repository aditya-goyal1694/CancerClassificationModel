df=read.csv("C:/Users/adity/Downloads/wisc_bc_data.csv")
head(df)
str(df)
df=df[-1]
table(df$diagnosis)
df$diagnosis[df['diagnosis']=='B']="Benign"
df$diagnosis[df['diagnosis']=='M']="Malignant"
df$diagnosis
summary(df[, c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_normalized <- as.data.frame(lapply(df[, c("radius_mean", "area_mean", "smoothness_mean")], normalize))
df_normalized

df_train<-df_normalized[1:270,]
df_test<-df_normalized[270:333,]
train_labels=df$diagnosis[1:270]
test_labels=df$diagnosis[270:333]

knn_prediction<-knn(train=df_train, test = df_test, k=21, cl=train_labels)
CrossTable(x=test_labels, y=knn_prediction, prop.chisq=FALSE)
