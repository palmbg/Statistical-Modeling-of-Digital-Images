library(magick)

# 1) Cria a imagem vazia
img <- image_blank(300, 300, color = "black")

# 2) Entra no modo de desenho e desenha diretamente
img <- image_draw(img)

# Desenha formas diretamente - sem plot.new()
rect(30, 170, 140, 280, col="white", border=NA)   # quadrado grande
rect(60, 200, 90, 230,  col="black", border=NA)   # furo preto
rect(170, 60, 280, 150, col="white", border=NA)   # retângulo
points(runif(12, 10, 290), runif(12, 10, 290), pch=16, cex=0.6, col="white") # ruído

dev.off()  # <- sai do modo de desenho e "salva" o resultado

# 3) Agora você já pode aplicar morfologia
# kernels: "Square:5","Disk:5", "Diamond:5", "Octagon:5", "Rectangle:9x3", "Cross:5", "Plus:5"
se <- "Square:5"
erosao <- image_morphology(img, method = "Erode", kernel = se)
print(erosao)
dilatacao  <- image_morphology(img, method = "Dilate", kernel = se)
print(dilatacao)
abertura   <- image_morphology(img, method = "Open",   kernel = se)
print(abertura)
fechamento <- image_morphology(img, method = "Close",  kernel = se)
print(fechamento)
