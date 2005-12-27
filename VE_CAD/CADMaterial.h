#ifndef CAD_MATERIAL_H
#define CAD_MATERIAL_H

/*!\file CADMaterial.h
 * CADMaterial API
 */

/*! \class VE_CAD::CADMaterial
 * Class to represent a basic material.
 */
namespace VE_XML{
   class VEFloatArray;
};
namespace VE_CAD{
class VE_CAD_EXPORTS CADMaterial: public VEXMLObject{
public:
   ///Constructor
   CADMaterial();
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
   std::vector<float> _kDiffuse;///< Diffuse component
   std::vector<float> _kEmission;///< Emmisive component
   std::vector<float> _ambient;///< Ambient component
   float _specular;///< specular component
};
}
#endif CAD_MATERIAL_H
