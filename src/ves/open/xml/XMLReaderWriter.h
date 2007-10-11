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

#ifndef XML_READER_WRITER_H
#define XML_READER_WRITER_H

/*!\file XMLReaderWriter.h
  XMLReaderWriter API
  */

/*!\class VE_XML::XMLReaderWriter
 *Abstract class to set things up for reading/writing VE-Suite XML files .
 * This class is the base class for representing
 * the hierarchy of a CAD structure.
 */

namespace VE_XML
{
   class XMLObject;
   class DOMDocumentManager;
}

#include "ves/VEConfig.h"
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>
#include <utility>


XERCES_CPP_NAMESPACE_USE

namespace VE_XML
{
class VE_XML_EXPORTS XMLReaderWriter
{
public:
   ///Default Constructor
   XMLReaderWriter();
   
   ///Destructor
   virtual ~XMLReaderWriter();

   ///Read an input XML string or file.
   ///\param xmlData The input XML string or file.
   ///\param objectNamespace The namespace of the object to extract.
   ///\param tagName The tagname of the element to extract.
   virtual void ReadXMLData( std::string xmlData,
                             std::string objectNamespace,
                             std::string tagName );

   ///Get all the element types in the document passed in
   ///based on user input
   ///\param xmlData The input XML string or file.
   ///\param elementTypes Vector of pairs of data types requested.
   virtual void ReadXMLData( std::string xmlData,
            std::vector< std::pair< std::string, std::string > > elementTypes );
                                     
   ///Write the current XML document
   ///\param xmlFile The XML document to write to.
   ///\param nodes The XML node to write. If writing to string set this to "returnString" and it
   ///will be populated w/ the return string.
   ///\param documentType The type of dom doc to be created either: Network, Shader, Command.
   virtual void WriteXMLDocument( std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes,
                                  std::string& xmlFile,
                                  std::string documentType );

   ///Write the current XML document out to multiple documents
   ///This will allow future extension to parallel reading of domcouments
   ///as well as provide support for web based extensions
   ///Note that this method only works for writing to disk no to memory
   ///\param xmlFile The base XML document to write to.
   ///\param nodes The XML node to write. 
   void WriteMultipleXMLDocuments( std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes,
                                                    std::string& xmlData );
   ///Set the Active DOMDocumentManager
   void SetDOMDocumentManager(VE_XML::DOMDocumentManager* ddManager);

   ///Turn on a stand alone DOMDocumentManager.
   void UseStandaloneDOMDocumentManager();

   ///Work around for the conflict when exposing the DOMDocumentManager
   void ReadFromFile();

   ///Work around for the conflict when exposing the DOMDocumentManager
   void ReadFromString();

   ///Work around for the conflict when exposing the DOMDocumentManager
   //void WriteToFile();

   ///Work around for the conflict when exposing the DOMDocumentManager
   //void WriteToString();
   
   ///Get the active DOMDocumentManager.  
   VE_XML::DOMDocumentManager* GetDOMDocumentManager();

   ///Return the loaded XMLObject s.
   std::vector<VE_XML::XMLObject*> GetLoadedXMLObjects();

protected:

   bool _standAloneDDM;///<Tells Reader whether it is using it's on DDM or one was passed in.

   ///Internal function to populate the appropriate structures from the file
   ///read in.
   ///\param rootDocument The document representing the input XML structure.
   ///\param objectNamespace The namespace the object to populate belongs to.
   ///\param tagName The tag name of the object to populate.
   virtual void _populateStructureFromDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument,
                                                std::string objectNamespace,
                                                std::string tagName );

    ///The XMLObjects read in from a document file.
    std::vector<VE_XML::XMLObject*> m_internalXmlObjects;
    ///The XMLObjects read in from a document file.
    std::vector<VE_XML::XMLObject*> m_xmlObjects;

   VE_XML::DOMDocumentManager* _domDocumentManager;///<The XML document manager.
};
}
#endif// XML_READER_WRITER_H
