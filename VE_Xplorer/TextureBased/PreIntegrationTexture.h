#ifndef PREINTEGRATION_TEXTURE_2D_H
#define PREINTEGRATION_TEXTURE_2D_H

#include <osg/Image>
#include <osg/Texture2D>
class TransferFunction;
class PreIntegrationTexture2D
{
public:
   ///Constructor
   PreIntegrationTexture2D();

   ///Copy Constructor
   PreIntegrationTexture2D(const PreIntegrationTexture2D& rhs);

   ///Destructor
   virtual ~PreIntegrationTexture2D();

   ///Set the transfer function for preintegrations
   ///\param tf The 1D transfer function
   void SetTransferFunction(TransferFunction* tf);

   ///Update the 2D preintegration texture
   void Update();

   ///\return The Pre-Integrated texture
   osg::Texture2D* GetPreIntegratedTexture();

   ///equal operator
   ///\param rhs The right hand side
   PreIntegrationTexture2D& operator=(const PreIntegrationTexture2D& rhs);
protected:
   ///Initialize the front and back integration values based on 
   ///the current transfer function data
   void _initializeSliceIntegrationValues();

   ///Calculate a specific component for the preintegration table
   ///\param ds Distance between slices
   ///\param component The rgba component to calculate 
   ///\param sliceMin The minimum slice value
   ///\param sliceMax The maximum slice value
   unsigned char _calculateComponent(float ds, unsigned int component,
                            unsigned int sliceMin, unsigned int sliceMax);

   TransferFunction* _tf;///<The 1D transfer function
   float* _sliceIntegrationValues;///<The preintegrated values
   unsigned char* _rawData;///<The raw texture data 
   osg::ref_ptr<osg::Image> _imageData;///<The image
   osg::ref_ptr<osg::Texture2D> _preIntegratedTexture;///<The image
};
#endif //PREINTEGRATION_TEXTURE_2D_H
