/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: VE2DDoubleArray.h,v $
 * Date modified: $Date: 2006-01-14 18:41:24 -0600 (Sat, 14 Jan 2006) $
 * Version:       $Rev: 3503 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_2DDOUBLE_ARRAY_H_
#define _XML_VE_2DDOUBLE_ARRAY_H_
/*!\file VE2DDoubleArray.h
  Double Array API
  */
/*!\class VE_XML::VE2DDoubleArray
 *This class basically manages a vector of floats. 
 */
#include <vector>

#include "VE_Open/VE_XML/VEXMLObject.h"

#include <xercesc/dom/DOM.hpp>
#include <iostream>
namespace VE_XML
{
   class VE1DDoubleArray;
}

namespace VE_XML
{
class VE_XML_EXPORTS VE2DDoubleArray : public VEXMLObject
{
public:
   ///Constructor
   ///\param rootDoc The owning DOMDocument.
   ///\param nElements The length of the float array.
   VE2DDoubleArray(DOMDocument* rootDoc,unsigned int nElements=3);
   ///Destructor
   virtual ~VE2DDoubleArray();
   ///Copy Constructor
   VE2DDoubleArray( const VE2DDoubleArray& );
   ///equal operator
   VE2DDoubleArray& operator= ( const VE2DDoubleArray& );

   ///Add a new element to the end of this array
   ///\param newValue The new value to add.
   void AddElementToArray( std::vector< double > newValue);

   ///Add a new element to the end of this array
   ///\param newValue The new value to add.
   void AddElementToArray( VE1DDoubleArray* newValue);

   ///Set this array from an input vector
   ///\param newArrayValues The new values to set to this array.
   void SetArray( std::vector< std::vector< double > > newArrayValues);

   ///Get a specific element
   ///\param i The i'th index of the element to return
   ///\param j The j'th index of the element to return
   double GetElement(unsigned int i, unsigned int j);

   ///Get the internal array.
   std::vector< std::vector< double > > GetArray( void );
   
   ///Populate the VEXMLObject data from an XML element.
   ///\param inputXML The input data.
   virtual void SetObjectFromXMLData( DOMNode* inputXML ); 
   
protected:
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName);
   unsigned int _nElements;///<Length of this float array.
   //std::vector<double> _array;///<Raw data.
   std::vector< std::vector< double > > doubleArray;///<Raw data.
   std::vector< VE1DDoubleArray* > ve1DDoubleArray;///<Raw data.

private:
   XMLSize_t minIndex;///<Mininum size of the array.
};
}
#endif// _XML_VE_2DDOUBLE_ARRAY_H_
