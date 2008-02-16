/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef _VES_OPEN_XML_CONVERT_H_
#define _VES_OPEN_XML_CONVERT_H_

#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include <sstream>
#include <iomanip>
#include <iostream>


namespace ves
{
namespace open
{
namespace xml
{

/*!\file Convert.h
  Xerces helper class for converting strings
  */
/*!\class ves::open::xml::Convert
 * Utility class to convert strings to Xerces compatible strings.
 */
class VE_XML_EXPORTS Convert
{

public:

   ///Constructor
   ///\param val The input to translate.
   template<typename T>
   Convert(const T& val)
   {
      std::stringstream ss;
      ss << std::setprecision( 10 ) << val;
      //std::cout << val << std::endl;
      mXmlUnicodeString = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode(
                              ss.str().c_str()
                              );
   }

   // Destructor
   ~Convert()
   {
      XERCES_CPP_NAMESPACE_QUALIFIER XMLString::release( &mXmlUnicodeString );
   }

   ///Get the XMLCh for the string.
   const XMLCh* toXMLString()
   {
      return mXmlUnicodeString;
   }
private:
   // -----------------------------------------------------------------------
   //  Private data members
   //
   //  mXmlUnicodeString
   //      This is the Unicode XMLCh format of the string.
   // -----------------------------------------------------------------------
   XMLCh*   mXmlUnicodeString;///< The raw unicode string.
};

}
}
}
#endif// _VES_OPEN_XML_CONVERT_H_
