/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_POINT_H_
#define _XML_VE_POINT_H_
/*!\file Point.h
  2D Point API
  */
/*!\class VE_XML::VE_Model::Point
 *This class basically manages two unsigned ints for constructing points
 *on the design canvas. 
 */
#include <utility>

#include <ves/open/xml/XMLObject.h>

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
namespace VE_Model
{
class VE_MODEL_EXPORTS Point : public VE_XML::XMLObject
{
public:
   ///Constructor
   Point(  );
   ///Destructor
   virtual ~Point();
   ///Copy Constructor
   Point( const Point& );
   ///equal operator
   Point& operator= ( const Point& );

   ///Set this array from an input vector
   ///\param newPoint The new values to set to this point.
   void SetPoint( std::pair< unsigned int, unsigned int > newPoint);

   ///Get the internal data.
   std::pair< unsigned int, unsigned int > GetPoint( void );
   
   ///Populate the VEXMLObject data from an XML element.
   ///\param inputXML The input data.
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* inputXML ); 
   
protected:   
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName);
   ///point holds two unsigned ints used by wx to construct
   ///the network diagram in Conductor
   ///The first component is x, the second component is y
   std::pair< unsigned int, unsigned int > point;///<Raw data.
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, VE_Model::Point* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif// _XML_VE_POINT_H_
