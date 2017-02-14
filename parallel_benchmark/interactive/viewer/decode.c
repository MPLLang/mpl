#include <stdio.h>
#include <jpeglib.h>
#include <stdlib.h>

typedef struct
{
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  FILE *file;
} img_data;

img_data *init_img(char *file)
{

  img_data *d = malloc(sizeof(img_data));
  d->file = fopen (file, "r");

  if (d->file == NULL)
    return 0;

  /*Initialize, open the JPEG and query the parameters */
  d->cinfo.err = jpeg_std_error(&(d->jerr));
  jpeg_create_decompress(&(d->cinfo));
  jpeg_stdio_src(&(d->cinfo), d->file);
  jpeg_read_header(&(d->cinfo), TRUE);
  jpeg_start_decompress(&(d->cinfo));

  return d;
}

int32_t decode(img_data *d, char *image)
{

  /* allocate data and read the image as RGBRGBRGBRGB */
  //image = malloc(cinfo.output_width * cinfo.output_height * 3);
  int i = 0;
  for(i = 0; i < d->cinfo.output_height; i++)
    {
      unsigned char * ptr = image + i * 3 * d->cinfo.output_width;
      jpeg_read_scanlines(&(d->cinfo), &ptr, 1);
    }

  /*Write a PPM */
  //printf("P6\n%i %i 255\n", cinfo.output_width, cinfo.output_height);
  //fwrite(image, 1, cinfo.output_width * cinfo.output_height * 3, stdout);

  return 1;
}

int32_t width(img_data *d)
{
  return d->cinfo.output_width;
}

int32_t height(img_data *d)
{
  return d->cinfo.output_height;
}


void finish(img_data *d)
{
  jpeg_finish_decompress(&(d->cinfo));
  jpeg_destroy_decompress(&(d->cinfo));
  fclose(d->file);

  free(d);
}
