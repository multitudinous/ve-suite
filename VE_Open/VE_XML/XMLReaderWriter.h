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
 * File:          $RCSfile: XMLReaderWriter.h,v $
 * Date modified: $Date: 2005-08-24 19:07:18 -0500 (Wed, 24 Aug 2005) $
 * Version:       $Rev: 2970 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef XML_READER_WRITER_H
#define XML_READER_WRITER_H

/*!\file XMLReaderWriter.h
  XMLReaderWriter API
  */

/*!\class XMLReaderWriter
 *Abstract class to set things up for reading/writing VE-Suite XML files .
 * This class is the base class for representing
 * the hierarchy of a CAD structure.
 */

namespace VE_XML{
   class DOMDocumentManager;
}
#include <xercesc/dom/DOM.hpp>
#include <string>
#include "VE_Installer/include/VEConfig.h"

XERCES_CPP_NAMESPACE_USE

namespace VE_XML{
class VE_XML_EXPORTS XMLReaderWriter{
public:
   ///Default Constructor
   XMLReaderWriter();
   
   ///Destructor
   virtual ~XMLReaderWriter();

   ///Read an input XML string of type.
   ///\param xmlData The input XML type.
   virtual void ReadXMLData(std::string xmlData);

   ///Write the current XML document
   ///\param xmlData The XML document to write to.
   virtual void WriteXMLDocument(std::string xmlData);

   ///Set the Active DOMDocumentManager
   void SetDOMDocumentManager(VE_XML::DOMDocumentManager* ddManager);

   ///Turn on a stand alone DOMDocumentManager.
   //Kinda a HACK!!!
   void UseStandaloneDOMDocumentManager();

   //Work around for the conflict when exposing the DOMDocumentManager
   void ReadFromFile();

   //Work around for the conflict when exposing the DOMDocumentManager
   void ReadFromString();

   //Work around for the conflict when exposing the DOMDocumentManager
   void WriteToFile();

   //Work around for the conflict when exposing the DOMDocumentManager
   void WriteToString();
   
   ///Get the active DOMDocumentManager.  
   VE_XML::DOMDocumentManager* GetDOMDocumentManager();

protected:

   bool _standAloneDDM;///<Tells Reader whether it is using it's on DDM or one was passed in.

   ///Internal function to populate the appropriate structures from the file
   ///read in.
   ///\param rootDocument The document representing the input XML structure.
   virtual void _populateStructureFromDocument(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument)=0;

   VE_XML::DOMDocumentManager* _domDocumentManager;///<The XML document manager.
};
}
#endif// XML_READER_WRITER_H
