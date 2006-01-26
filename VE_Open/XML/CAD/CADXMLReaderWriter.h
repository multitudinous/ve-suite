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
 * File:          $RCSfile: CADXMLReaderWriter.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_XML_FILE_READER_H
#define CAD_XML_FILE_READER_H

/*!\file CADXMLReaderWriter.h
  CADXMLReaderWriter API
  */

/*!\class VE_CAD::CADXMLReaderWriter
 * Class to read in a CAD XML file.
 */
#include "VE_Open/XML/XMLReaderWriter.h"
namespace VE_CAD
{
   class CADNode;
}
XERCES_CPP_NAMESPACE_USE
namespace VE_CAD{
   class VE_CAD_EXPORTS CADXMLReaderWriter: public VE_XML::XMLReaderWriter{
public:
   ///Default Constructor
   CADXMLReaderWriter();
   
   ///Copy Constructor
   CADXMLReaderWriter(const CADXMLReaderWriter& fr);

   ///Destructor
   virtual ~CADXMLReaderWriter();

   ///\param cadNode The cadNode to write.
   void SetCADNode(CADNode* cadNode);

   ///Get the root node of the CAD Hierarchy
   VE_CAD::CADNode* GetRootNode();

   ///Equal Operator
   CADXMLReaderWriter& operator=(const CADXMLReaderWriter& rhs);
protected:
   ///Internal function to populate the appropriate structures from the file
   ///read in.
   ///\param rootDocument The document representing the input XML structure.
   virtual void _populateStructureFromDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument);
   VE_CAD::CADNode* _rootNode;///< The rootNode of the XML Data read in.
};
}
#endif// CAD_XML_READER_H
