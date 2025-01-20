# Feeder Main

Rodrigo Dal Ben, 2025-01-20

## An adventure in Generative Art

### Inspiration
In 2024, a gigantic puddle appeared in a Calgary intersection. Hours later, the City found that [Calgary's Bearspaw South Feeder Main had a severe water leak](https://globalnews.ca/news/10626802/calgary-feeder-main-break-ballpark-cost/).
Further investigation revealed that most of the [several causes of the dramatic failure](https://www.cbc.ca/news/canada/calgary/calgary-bearspaw-south-feeder-main-preliminary-findings-1.7385334#:~:text=Built%20in%201975%2C%20the%20feeder,the%20city's%20treated%20water%20supply.) were the result of small changes to the feeder structure over a long period of time.
While saving water and wondering whether that was a glimpse of an apocalyptical world, I couldn't help but make the parallel with our own feeder main: our spinal cord.
We all relay deeply on it, its maintenance is often neglect, and several small changes over a long period of time can cause problems that affect our whole health. 

### Motivation
A series of [blog posts by Nicola Rennie](https://nrennie.rbind.io/blog/getting-started-generative-art/) 
and the [workshop materials from Daniele Navarro](https://art-from-code.netlify.app/) got me wondering whether me, a mere mortal like me, would be able to turn data frames into art. 
Following this [hands-on step-by-step tutorial by Michael Freeman](http://mfviz.com/r-image-art/) I got interested in creating a [Vonoroi diagram](https://en.wikipedia.org/wiki/Voronoi_diagram), a kind of tessellation, of our feeder main.
The [lateral X-ray of the neck with cervical collar in a patient with Cervical Spine Trauma by Nevit Dilmen, CC-BY-SA 3.0](https://commons.wikimedia.org/wiki/File:Medical_X-Ray_imaging_RAH06_nevit.jpg) 
and the amazing packages [ggvoronoi](https://github.com/garretrc/ggvoronoi), [cropcircles](https://github.com/doehm/cropcircles), [imager](https://asgr.github.io/imager/), [magick](https://docs.ropensci.org/magick/articles/intro.html#read-and-write), made the dream come true.

I hope you enjoy the art piece and please make use of the open source code that comes with it to start your own adventures into generative art! 

***

## Folder structure

`input` 

  1. `00`: Original image (`.jpg`)
  2. `01`: Cropped images into a square (`.jpg`)
  3. `02`: Square image with removed background and resize to 500x500 pixels (`.png`)
  4. `03`: Square image with solid, black, background (`.jpg`)
  5. `qr_code`: QR code with signature, generated by the code (`.svg`)

`output`

  1. `01`: Voronoi diagram of the input image (`.jpeg`)
  2. `02`: Voronoi croped into a circle (`.jpeg`)
  3. `03`: Final Vonoroi with QR code signature (`.jpeg`)
     
`script`: R function, `fnc_voronoi`, to recreate the Feeder Main. 

### License

**CC-BY-SA 4.0** (see `license.md`)
