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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _VE_MODEL_H_
#define _VE_MODEL_H_
/*!\file Model.h
  *Model API
  */

/*!\class VE_Model::Model
 *Class that manages the model info for all of VE-Suite.
 */

/*!\namespace VE_Model
 * Contains nodes for creating/managing a Model data.
 */
#include <string>
#include <vector>
#include "VE_Open/XML/XMLObject.h"

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
   class DataValuePair;
   class Command;
   class ParameterBlock;
   namespace VE_Model
   {
      class Port;
      class Point;
   }
}

namespace VE_CAD
{
   class CADNode;
   class CADAssembly;
}

namespace VE_XML
{
namespace VE_Model
{
class VE_MODEL_EXPORTS Model : public VE_XML::XMLObject
{
public:
   ///Constructor
   Model( );
   ///Destructor
   virtual ~Model();
   ///Copy Constructor
   Model( const Model& );
   ///equal operator
   Model& operator= ( const Model& );
   
   ///Set the model name
   ///\param name name of the model
   void SetModelName( std::string name );
   ///Set the unique model id
   ///\param id id of the model
   void SetModelID( unsigned int id );
   ///Set the icon filename 
   ///\param filename filename for the icon to be loaded
   void SetIconFilename( std::string filename );
   
   ///set the data from an string representing the xml
   ///\param xmlInput The input XML data.
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput);
   
   ///Get the model name
   std::string GetModelName( void );
   ///Get the model id
   unsigned int GetModelID( void );
   ///Get the icon file name
   std::string GetIconFilename( void );

   ///Get point for the icon location
   Point* GetIconLocation( void );
   ///Get results data
   VE_XML::Command* GetResult(int i );
   ///Get results data
   size_t GetNumberOfResults( void );
   ///Get input data
   ///\param i get the i'th input, to allocate a new DataValuePair pass in -1
   VE_XML::Command* GetInput( int i );

   ///Get the input variable by name
   ///\param inputName Then name of the input to retrieve
   VE_XML::Command* GetInput(std::string inputName);

   ///Allocate another input block for use
   VE_XML::Command* GetInput( void );
   ///Get number of input data
   size_t GetNumberOfInputs( void );
   ///Get the i'th port for the model.
   ///\param i The i'th port you are after.
   Port* GetPort(int i );
   ///Allocates a new port for the model.
   Port* GetPort( void );
   ///Get port data
   size_t GetNumberOfPorts( void );
   /*///Get the i'th input port for the model.
   ///\param i The i'th input port you are after.
   Port* GetInputPort( unsigned int i );
   ///Get the number of input ports for a model
   size_t GetNumberOfInputPorts( void );
   ///Get the i'th output port for the model.
   ///\param i The i'th output port you are after.
   Port* GetOutputPort( unsigned int i );
   ///Get the number of output ports for a model
   size_t GetNumberOfOutputPorts( void );*/
   ///Get the i'th information packet for a model.
   ///\param i The i'th packet you are after.
   VE_XML::ParameterBlock* GetInformationPacket( int i );
   ///Get info packets data
   size_t GetNumberOfInformationPackets( void );
   ///Get the geometry for the model.
   VE_CAD::CADNode* GetGeometry( void );
   ///Remove the i'th information packet for a model.
   ///\param i The i'th packet you are after.
   void RemoveInformationPacket( unsigned int i );
   ///Add a geometry node and return it
   /// if there is already geometry then the function will return that pointer
   VE_CAD::CADNode* Model::AddGeometry( void );
   ///Delete the geometry for this model
   void DeleteGeometry( void );
   ///Set the vendor name on the model
   void SetVendorName( std::string vendorName );
   ///Get the vendor name of the model
   std::string GetVendorName( void );

protected:
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName );

private:
   ///raw datatypes of Model that are specified in the verg_model.xsd file
   std::string modelName;///<The name of the model.
   unsigned int uniqueModelID;///<The unique model id. 
   std::string iconFileName;///<The filename for the icon if any at all. Can possibly use an which would be compiled at runtime.
   std::vector< Port* > ports;///<The vector port data if any for a model.
   std::vector< Port* > outputPorts;///<The vector port data if any for a model.
   std::vector< Port* > inputPorts;///<The vector port data if any for a model.
   Point* iconLocation;///<The icon location point container.
   ///The data value pair will contain all the results for a paticular model
   std::vector< VE_XML::Command* > results;///<The classes hold the results for the model.
   ///The data value pair will contain the model inputs for the model
   std::vector< VE_XML::Command* > inputs;///<The classes hold the inputs for the model.
   ///The parameter block holds all the data the was formerly stored in the param file
   std::vector< VE_XML::ParameterBlock* > informationPackets;///<The classes hold relevant data to represent the model.
   ///The CADNode contains the tree structure for the geometry
   VE_CAD::CADAssembly* geometry;///<The classes hold the geometry for the model.
   ///The vendor name for the model
   std::string vendorUnit;///<The string that stores the unit name that this model maps to
};
}
}

#endif// _VE_MODEL_H_
