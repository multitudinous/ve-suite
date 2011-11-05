#include <iostream>

#include <png.h>
//http://stackoverflow.com/questions/2442335/libpng-boostgil-png-infopp-null-not-found
#if PNG_LIBPNG_VER > 10210
#define png_infopp_NULL (png_infopp)NULL
#define int_p_NULL (int*)NULL
#endif

#include <boost/gil/gil_all.hpp>
#include <boost/gil/image.hpp>

#include <boost/gil/extension/io/dynamic_io.hpp>
#include <boost/gil/extension/io/png_dynamic_io.hpp>
#include <boost/gil/extension/io/png_io.hpp>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <boost/lexical_cast.hpp>

boost::gil::rgb8_image_t image;
boost::gil::rgb8_image_t whiteImage;
boost::gil::rgb8_pixel_t pixel;
boost::gil::rgb8_image_t blobImage;
boost::gil::rgb8_image_t visitedImage;
boost::gil::rgb8_image_t smallImage;

boost::gil::rgb8_pixel_t whitePixel( 255, 255, 255 );;
boost::gil::rgb8_pixel_t blackPixel( 0, 0, 0 );
double r,g,b;
size_t blobSize = 0;
boost::gil::point2< int > min;
boost::gil::point2< int > max;

void extractblob( int x, int y, int currentpositionx, int currentpositiony ) 
{
    ///make sure it is a valid pixel first
    if( x < 0 || y < 0 )
    {
        return;
    }

    ///make sure it is a valid pixel first
    if( (x + 1 > image.width()) || (y + 1 > image.height()) )
    {
        return;
    }

    if( visitedImage._view(x,y) == whitePixel )
    {
        return;
    }

    pixel = image._view(x,y);
    r = double(boost::gil::get_color(pixel, boost::gil::red_t()));
    g = double(boost::gil::get_color(pixel, boost::gil::green_t()));
    b = double(boost::gil::get_color(pixel, boost::gil::blue_t()));

    //mark visited
    visitedImage._view(x,y) = whitePixel;

    if( (r>220) && (g>220) && (b>220) )
    {
        //<< boost::gil::get_color(pixel, boost::gil::alpha_t()) << std::endl;
        return;
    }
    blobSize += 1;
    
    if( x < min.x )
    {
        min.x = x;
    }
    
    if( x > max.x )
    {
        max.x = x;
    }

    if( y < min.y )
    {
        min.y = y;
    }
    
    if( y > max.y )
    {
        max.y = y;
    }

    //std::cout << r << " " << g << " " << b << " "<< std::endl; 
    //std::cout << currentpositionx << " " << currentpositiony << std::endl;
    //copy(buffer[x][y], blobbuffer[currentpositionx][currentpositiony])
    blobImage._view( currentpositionx, currentpositiony ) = pixel;

    extractblob( x-1,   y, currentpositionx-1,   currentpositiony);
    extractblob( x+1,   y, currentpositionx+1,   currentpositiony);
    extractblob(   x, y+1,   currentpositionx, currentpositiony+1);
    extractblob(   x, y-1,   currentpositionx, currentpositiony-1);
}
        
int main( int argc, char* argv[] )
{
    boost::filesystem::path scalarPath( argv[ 1 ] );
    //Get base directory name from vtiFilename
    std::string const directory = scalarPath.remove_filename().string();

    //Read in the png image
    boost::gil::png_read_image( argv[ 1 ], image);
    //loop over all of the pixels
    //boost::gil::rgba8c_view_t view( image._view );

    visitedImage.recreate( image.dimensions(), blackPixel, 1 );
    blobImage.recreate( image.dimensions(), whitePixel, 1 );
    whiteImage.recreate( image.dimensions(), whitePixel, 1 );

    size_t counter = 0;
    for (int y=0; y<image._view.height(); ++y)
    {
        for (int x=0; x<image._view.width(); ++x)
        {
            if( visitedImage._view(x,y) == blackPixel )
            {
                blobSize = 0;
                min.x = 100000000;
                min.y = 100000000;
                max.x = 0;
                max.y = 0;

                extractblob( x, y, x, y );
                if( blobSize > 200 )
                {
                    //std::cout << min.x << " " << min.y << std::endl << max.x << " " << max.y << std::endl << std::endl;
                    //std::cout << max.x - min.x << " " << max.y - min.y << std::endl << std::endl;
                    int dim1 = max.x - min.x;
                    int dim2 = max.y - min.y;
                    smallImage.recreate( dim1+10, dim2+10, whitePixel, 1 );
                    
                    boost::gil::copy_pixels( 
                        boost::gil::subimage_view( boost::gil::view( blobImage ), min.x, min.y, dim1, dim2), 
                        boost::gil::subimage_view( boost::gil::view( smallImage ), 5, 5, dim1, dim2) );
                    
                    std::string fileName = directory + "/blob_" + boost::lexical_cast< std::string >( counter ) + ".png";
                    boost::gil::png_write_view( fileName, boost::gil::view( smallImage ) );
                    
                    //blobImage = whiteImage;
                    boost::gil::fill_pixels( boost::gil::view( blobImage ), whitePixel );

                    counter += 1;
                }
            }
        }
    }
    std::cout << counter << " blobs found." << std::endl;

    return 0;
}

/*
 recusive function passes
 
 x, y, 
 
 once recusive function scan the white error
 x, y, 
 
 2 output buffers
 
 1 visited period
 
 1. current output buffer
 
 
 overall search function
 
 
 
 //dont do this recursively just do a search, making sure to respect the visited marker as set by extractblob
 findnextblob(x,y) {
 if I've been visited before or I'm out of bounds or negative
 return;
 if I'm not white:
 extractblob(x,y, midofblobbuffer, midofblobbuffery)
 findextentsandsaveblobbuffer() //write to file because we extracted the entire blob
 clearblobbuffer()
 markpixelasvisitted
 findnextblob(x-1, y)
 findnextblob(x,y-1)
 findnextblbob(x+1, y)
 findnextblob(x, y+1)
 }
 
 extractblob(x,y,currentpositionx, currentpositiony) {
 if I'm white:
 mark visited
 return
 else
 copy(buffer[x][y], blobbuffer[currentpositionx][currentpositiony])
 extractblob(x-1,y, currentpositionx-1, currentpositiony)
 //do the other 3
 
 }
*/
