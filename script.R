library(magick)

# 1) Cria a imagem vazia
img = image_blank(300, 300, color = "black")

# 2) Entra no modo de desenho e desenha diretamente
img = image_draw(img)

# Desenha formas diretamente - sem plot.new()
rect(30, 170, 140, 280, col="white", border=NA) #quadrado 
rect(60, 200, 90, 230,  col="black", border=NA) #furo preto
rect(170, 60, 280, 150, col="white", border=NA) #retangulo
points(runif(12, 10, 290), runif(12, 10, 290), 
       pch=16, cex=0.6, col="white") #ruido

dev.off()  #sai do modo de desenho e "salva" o resultado

# 3) Agora você ja' pode aplicar morfologia
# kernels: "Square:5","Disk:5", "Diamond:5", "Octagon:5", 
# "Rectangle:9x3", "Cross:5", "Plus:5"
se = "Square:5"
erosao = image_morphology(img, method = "Erode", kernel = se)
print(erosao)
dilatacao = image_morphology(img, method = "Dilate", kernel = se)
print(dilatacao)
abertura = image_morphology(img, method = "Open",  kernel = se)
print(abertura)
fechamento = image_morphology(img, method = "Close", kernel = se)
print(fechamento)


# 4) Trabalhando com imagem SAR real (MSTAR dataset)
library(raster)
# Um raster e' uma representacao matricial de dados espaciais

# Funcao para ler imagem SAR em arquivo binario
read_mstar <- function(file_path, img_width, img_height) 
{
  con <- file(file_path, "rb") # Abre arquivo binario 
  raw_data <- readBin(con, what = "numeric", size = 4, 
                      n=img_width*img_height,endian="little")
  close(con) # fecha arquivo binario
  
  # Converter os dados para matriz
  img_matrix <- matrix(raw_data, nrow = img_height, 
                       ncol = img_width, byrow = T)
  return(img_matrix)
}

# Dimensao da imagem (matriz)
img_width<- 128
img_height<- 128

# Usando a funcao construida acima
im_matrix <- read_mstar("HB03333.001.mag",img_width,img_height)

# Plotando a imagem (nao e' boa forma)
image(im_matrix,col = gray.colors(256))
# image(1:img_width, 1:img_height, im_matrix,col = gray.colors(256)) # por pixels

# Melhor forma de plotar (usando raster)
plot(raster(im_matrix), col = gray.colors(256))

hist(as.vector(im_matrix),breaks =50)
thresh<- 0.11 # threshold (limiar). Ex: thresh<- 0.11 ou 0.2
abline(v=thresh,col="red")

# Pixels detectados
detected<- ifelse(im_matrix<thresh,0,1)
plot(raster(detected), col = gray.colors(256))

# Operacoes morfologicas com magick
# Converte para uma imagem magick
img <- image_read(as.raster(detected))
plot(img)

# Operacoes
se = "Square:5"
fechamento = image_morphology(img,method="Close",kernel=se)
plot(fechamento)
abertura = image_morphology(fechamento,method="Open",kernel=se)
plot(abertura) # deteccao final do alvo

# Extrair caracteristicas do alvo (futura classificacao)
library(EBImage)
# Transformando objeto magick em EBImage
abertura_raster<- as.raster(abertura) 
abertura_array <- col2rgb(abertura_raster)/255
abertura_matrix <- matrix(abertura_array[1,], 
                          nrow = attr(abertura_raster,"dim")[1])
alvo <- Image(abertura_matrix, colormode = "Grayscale")

# Rotular os objetos na imagem binaria
labels <- bwlabel(alvo)
features <- computeFeatures(labels,im_matrix)
print(features) # varios tipos de features
colnames(features)
# b.* = basic features (baseadas em intensidade da imagem)
# s.* = shape features (geometria)
# m.* = moment features (momentos de ordem superior)
# h.* = haralick features (textura)

# Exemplo: area e media dos valores do alvo
features[,"x.0.s.area"]
features[,"x.a.b.mean"]

# Exemplo: Agrupamento (aprendizado nao supervisionado)
# Calcular a matriz de distancias
d <- dist(features)

# Realizar o clustering hierarquico
hc <- hclust(d)

# Plotar o dendrograma
plot(hc)

# Selecionando alguns pontos na imagem
library(pracma) # poligono
plot(raster(im_matrix), col = gray.colors(256))
title("Clique nos 4 pontos do quadrilatero")

# Captura os 4 pontos clicados pelo usuario
quad_points <- locator(4,type ="p",col="red",pch=16,cex=1.5)

# Converte para um data frame
quadrilatero <- data.frame(
  x = quad_points$x,  
  y = quad_points$y
)

# Exibe as coordenadas dos pontos selecionados
print(quadrilatero)

# Plota imagem novamente
plot(raster(im_matrix), col = gray.colors(256))
# Desenha o quadrilatero sobre a imagem
polygon(quadrilatero$x,quadrilatero$y,border="red",lwd=2)


