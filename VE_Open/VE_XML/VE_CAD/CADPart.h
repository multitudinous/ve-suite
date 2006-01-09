/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: CADNodePart.h,v $
 * Date modified: $Date: 2005-07-11 13:47:16 -0500 (Mon, 11 Jul 2005) $
 * Version:       $Rev: 2653 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _CAD_PART_H_
#define _CAD_PART_H_
/*!\file CADPart.h
 * CADPart API
 */

/*!\class VE_CAD::CADPart 
 * Class to represent a part file (the actual CAD geometry)
 */
#include <xercesc/dom/DOM.hpp>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VE_CAD/CADNode.h"
#include <string>

XERCES_CPP_NAMESPACE_USE
namespace VE_CAD{
class VE_CAD_EXPORTS CADPart: public VE_CAD::CADNode{
public:
   ///Constructor
   ///\param rootDocument The root XML document 
   ///\param name The name of the part
   CADPart(DOMDocument* rootDocument,std::string name=std::string("Part"));

   ///Destructor
   virtual ~CADPart();

   ///\param cadFileName The name of the part
   ///Set the name of the CAD file this node represents
   void SetCADFileName(std::string cadFileName);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( DOMNode* xmlNode);

   ///Get the name of the CAD file this node represents
   std::string GetCADFileName();

   ///Copy constructor
   CADPart(const CADPart& rhs);

   ///Equal operator
   CADPart& operator=(const CADPart& rhs);
protected:
   

   ///Internally update the XML data for this node.
   ///\param input The XML data for this element.
   virtual void _updateVEElement(std::string input);

   ///Internally update the XML data for the CAD filename that this part represents.
   void _updateCADFileName();
   
   std::string _cadFileName; ///<The name of the CAD file on disk
};
}
#endif// _CAD_PART_H_
