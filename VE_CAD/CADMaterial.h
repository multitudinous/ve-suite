#ifndef CAD_MATERIAL_H
#define CAD_MATERIAL_H

#include "VE_Installer/include/VEConfig.h"
#include "VE_XML/VEXMLObject.h"
#include <xercesc/dom/DOM.hpp>

/*!\file CADMaterial.h
 * CADMaterial API
 */

/*! \class VE_CAD::CADMaterial
 * Class to represent a basic material.
 */
XERCES_CPP_NAMESPACE_USE
namespace VE_XML{
   class VEFloatArray;
};
namespace VE_CAD{
class VE_CAD_EXPORTS CADMaterial: public VEXMLObject{
public:
   ///Constructor
   ///\param rootDocument The root XML document of this material.
   ///\param name The name of this material.
   CADMaterial(DOMDocument* rootDocument,std::string name=std::string("Material"));
   ///Destructor
   virtual ~CADMaterial();

   ///Set the diffuse component
   ///\param diffuse RGBA diffuse property
   void SetDiffuseComponent(float* diffuse);

   ///Set the emissive component
   ///\param emissive RGBA emissive property
   void SetEmissiveComponent(float* emissive);

   ///Set the ambient component
   ///\param ambient RGBA ambient property
   void SetAmbientComponent(float* ambient);

   ///Set the specular reflection component
   ///\param specular "reflective" property
   void SetSpecularComponent(float specular);

   ///Get the diffuse property
   std::vector<float> GetDiffuse();
   ///Get the emissive property
   std::vector<float> GetEmissive();
   ///Get the ambient property
   std::vector<float> GetAmbient();
   ///Get the specular property
   float GetSpecular();
protected:	
   ///Internally update the XML data for the material.
   virtual void _updateVEElement();
   std::vector<float> _kDiffuse;///< Diffuse component
   std::vector<float> _kEmission;///< Emmisive component
   std::vector<float> _ambient;///< Ambient component
   float _specular;///< specular component
   std::string _materialName;///<The name of this material.
};
}
#endif CAD_MATERIAL_H
