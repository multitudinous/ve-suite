#ifndef _XML_VE_FLOAT_ARRAY_H_
#define _XML_VE_FLOAT_ARRAY_H_

#include <vector>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
class VE_XML_EXPORTS VEFloatArray : public VEXMLObject
{
public:
   VEFloatArray(DOMDocument* rootDoc,unsigned int nElements=3);
   virtual ~VEFloatArray();
   VEFloatArray( const VEFloatArray& );
   //equal operator
   VEFloatArray& operator= ( const VEFloatArray& );

   void AddElementToArray( double );
   void SetArray( std::vector<double> );

   double GetElement( unsigned int );
   std::vector<double> GetArray( void );
   virtual void SetObjectFromXMLData( DOMNode* ); 
   
protected:
   virtual void _updateVEElement( std::string );
   unsigned int _nElements;
   std::vector<double> _array;

private:
   XMLSize_t minIndex;
   XMLSize_t maxIndex;
};
}
#endif// _XML_VE_FLOAT_ARRAY_H_
