# Documentación trabajo de Tesis - Incorporación de energía solar y eólica en el sistema eléctrico colombiano. Una aplicación de la matriz insumo-producto para analizar la política del sector

## Maestría en Gobierno y Asuntos Públicos | Flacso México | 2022 - 2024

Durante los años 2022 y 2024 tuve la suerte de realizar la Maestría en Gobierno y Asuntos Públicos en Flacso México, una experiencia formativa muy enriquecedora de la mano de grandes profesores.  
En este repositorio se encuentra el procesamiento de la información y las visualizaciones generadas para el trabajo de tesis, todos fueron desarrollados en `R` y las visualizaciones se hicieron utilizando la librería `{ggplot2}`.

El trabajo de tesis es un ejercicio prospectivo que busca evaluar los efectos de la incorporación de proyectos solares y eólicos en la matriz energética de Colombia, utilizando la matriz Insumo-Producto (*I-O* por sus siglas en ingles), se genera evidencia sobre la correspondencia de la capacidad a instalar con las necesidades energéticas del país, teniendo en cuenta la demanda final y los consumos intermedios que realizan los distintos sectores de la economía colombiana.  
La conclusión del trabajo, es que los proyectos aprobados a diciembre de 2023, exceden las necesidades energéticas del país, esto si bien puede tener efectos positivos en los precios (por incrementos significativos en la oferta), tambien puede generar problemas en el funcionamiento del Sistema Interconectado Nacional (SIN) por la inestabilidad característica en la generación de electricidad a partir de energía solar y eólica. Adicionalmente, para la adecuada incorporación de estas fuentes, se requiere un conjunto de inversiones complementarias en la capacidad de transmisión y de distribución, las cuales se encuentran rezagadas al momento del estudio.

Se estimaron más de 20.000 escenarios de crecimiento para verificar la plausibilidad de consumir la electricidad que hipotéticamente generarían los proyectos, el procesamiento de la matriz I-O se realizó utilizando el paquete `ioanalysis` disponible en el CRAN de R.

Si tienes alguna inquietud o comentario respecto al contenido de este repositorio, contactame al correo <gomezsantiagogerardoesteban@gmail.com>

