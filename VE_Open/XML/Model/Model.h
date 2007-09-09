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
#ifndef VE_XML_VE_MODEL_MODEL_H_
#define VE_XML_VE_MODEL_MODEL_H_
/*!\file Model.h
  *Model API
  */

/*!\class VE_XML::VE_Model::Model
 *Class that manages the model info for all of VE-Suite.
 */

/*!\namespace VE_XML::VE_Model
 * Contains nodes for creating/managing a Model data.
 */
#include <string>
#include <vector>
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Model/SystemStrongPtr.h"
#include "VE_Open/XML/Model/SystemWeakPtr.h"

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
   namespace VE_CAD
   {
     class CADNode;
     class CADAssembly;
   }

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
    ///\return The model name
    std::string GetModelName( void );
    ///Get the model id
    ///\return The model ID used my third party solver
    unsigned int GetModelID( void );
    ///Get the icon file name
    ///\return The icon filename
    std::string GetIconFilename( void );
    ///Set the roation of the icon
    void SetIconRotation( float rotation );
    ///Get the icon rotation
    ///\return The icon rotation
    float GetIconRotation( void );
    ///Set the scale of the icon
    void SetIconScale( float scale );
    ///Get icon scale
    ///\return The icon scale
    float GetIconScale( void );
    ///Set the mirroed attribute of the icon
    void SetIconMirror( int mirror );
    ///is the icon mirrored
    ///\return The mirror flag for the icon
    int GetIconMirror( void );
    ///Get point for the icon location referenced to the upper left hand corner
    ///\return The point for the icon location
    Point* GetIconLocation( void );
    ///Get results data
    ///\return The ith result stored as a command
    VE_XML::Command* GetResult(int i );
    ///Get results data
    ///\return The number of results in this model
    size_t GetNumberOfResults( void );
    ///Get input data
    ///\param i get the i'th input, to allocate a new DataValuePair pass in -1
    ///\return The ith input in this model
    VE_XML::Command* GetInput( int i );

    ///Get the input variable by name
    ///\param inputName Then name of the input to retrieve
    ///\return The input with the given name
    VE_XML::Command* GetInput(std::string inputName);

    ///Allocate another input block for use
    ///\return The new input
    VE_XML::Command* GetInput( void );
    ///Get number of input data
    ///\return The total number of inputs
    size_t GetNumberOfInputs( void );
    ///Get the i'th port for the model.
    ///\param i The i'th port you are after.
    ///\return The ith port selected by the user
    Port* GetPort(int i );
    ///Allocates a new port for the model.
    ///\return The new port
    Port* GetPort( void );
    ///Get port data
    size_t GetNumberOfPorts( void );
    ///Remove the i'th port for the model.
    ///\param i The i'th port you are after.
    void RemovePort( unsigned int i );
    ///Remove the selected port for the model.
    ///\param removePort The port you are after.
    void RemovePort( VE_XML::VE_Model::Port* removePort );
    ///Get the i'th information packet for a model.
    ///\param i The i'th packet you are after.
    VE_XML::ParameterBlock* GetInformationPacket( int i );

    ///Get the named information packet for a model.
    ///\param name The name of the packet you are after.
    VE_XML::ParameterBlock* GetInformationPacket( std::string name);

    ///Get info packets data
    size_t GetNumberOfInformationPackets( void );
    ///Get the geometry for the model.
    VE_XML::VE_CAD::CADNode* GetGeometry( void );
    ///Remove the i'th information packet for a model.
    ///\param i The i'th packet you are after.
    void RemoveInformationPacket( unsigned int i );

    ///Remove the named information packet from a model.
    ///\param name The name of the packet to remove.
    void RemoveInformationPacket(std::string name );

    ///Add a geometry node and return it
    /// if there is already geometry then the function will return that pointer
    VE_XML::VE_CAD::CADNode* AddGeometry( void );
    ///Delete the geometry for this model
    void DeleteGeometry( void );
    ///Set the vendor name on the model
    void SetVendorName( std::string vendorName );
    ///Get the vendor name of the model
    std::string GetVendorName( void );
    ///Add a model attribute
    void SetModelAttribute( VE_XML::Command* modelAttribute );
    ///Get model attributes
    ///\return The model attributes used by third party solvers
    VE_XML::Command* GetModelAttribute( void );

    ///New interface for handling systems of systems
    ///Set the sub network for this model
    ///\param network The subnetwork for this model
    void SetSubSystem( VE_XML::VE_Model::SystemWeakPtr inputSystem );
    ///Get the sub network for this model
    ///\return The subnetwork for this model
    VE_XML::VE_Model::SystemWeakPtr GetSubSystem();
    
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
   Point* iconLocation;///<The icon location point container.
   ///The data value pair will contain all the results for a paticular model
   std::vector< VE_XML::Command* > results;///<The classes hold the results for the model.
   ///The data value pair will contain the model inputs for the model
   std::vector< VE_XML::Command* > inputs;///<The classes hold the inputs for the model.
   ///The parameter block holds all the data the was formerly stored in the param file
   std::vector< VE_XML::ParameterBlock* > informationPackets;///<The classes hold relevant data to represent the model.
   ///The CADNode contains the tree structure for the geometry
   VE_XML::VE_CAD::CADAssembly* geometry;///<The classes hold the geometry for the model.
   ///The vendor name for the model
   std::string vendorUnit;///<The string that stores the unit name that this model maps to
   VE_XML::Command* modelAttribute;///<The structure that stores all of the model attributes
   float iconScale;///<The icon scale to set the right size for conductor and xplorer
   float iconRotation;///<The icon rotation for conductor and xplorer
   ///the icon image needs to be mirrored 1 = no, 2 = horizontally, 3= vertically
   unsigned int iconMirror;
   ///The sub network for this model
   VE_XML::VE_Model::SystemStrongPtr m_subSystem;
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, VE_Model::Model* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif// _VE_MODEL_H_
