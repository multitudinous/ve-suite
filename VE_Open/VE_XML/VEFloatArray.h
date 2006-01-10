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
 * File:          $RCSfile: VEFloatArray.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

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
