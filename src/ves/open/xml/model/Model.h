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
#ifndef _VES_OPEN_XML_MODEL_MODEL_H_
#define _VES_OPEN_XML_MODEL_MODEL_H_

#include <ves/open/xml/model/ModelPtr.h>

#include <ves/open/xml/XMLObject.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/ParameterBlockPtr.h>
#include <ves/open/xml/model/SystemPtr.h>
#include <ves/open/xml/model/PortPtr.h>
#include <ves/open/xml/model/PointPtr.h>
#include <ves/open/xml/cad/CADNodePtr.h>
#include <ves/open/xml/cad/CADAssemblyPtr.h>

#include <xercesc/dom/DOM.hpp>

#include <boost/enable_shared_from_this.hpp>

#include <string>
#include <vector>
#include <map>

namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
/*!\file Model.h
  *Model API
  */
/*!\class ves::open::xml::model::Model
 *Class that manages the model info for all of VE-Suite.
 */
/*!\namespace ves::open::xml::model
 * Contains nodes for creating/managing a Model data.
 */
class VE_MODEL_EXPORTS Model : public ves::open::xml::XMLObject,
                               public boost::enable_shared_from_this<Model>
{

public:
    ///Constructor
    Model();
    ///Destructor
    virtual ~Model();
    ///Copy Constructor
    Model( const Model& );
    ///equal operator
    Model& operator= ( const Model& );

    ///Set the model name
    ///\param name name of the model
    void SetModelName( const std::string& name );

    ///Set the unique model id
    ///\param id id of the model
    vesDEPRECATED( void SetModelID( unsigned int id ) );

    ///Set the icon filename
    ///\param filename filename for the icon to be loaded
    void SetIconFilename( const std::string& filename );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the model name
    ///\return The model name
    const std::string& GetModelName( void );

    ///Get the model id
    ///\return The model ID used my third party solver
    vesDEPRECATED( unsigned int GetModelID( void ) );

    ///Get the icon file name
    ///\return The icon filename
    const std::string& GetIconFilename( void );

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

    ///set the icon hidden flag
    ///used in conductor to hide icons
    //necessary for standalone links
    void SetIconHiddenFlag( int flag );

    ///Get icon hidden flag
    ///\return The icon hidden flag
    int GetIconHiddenFlag( void );

    ///Get point for the icon location referenced to the upper left hand corner
    ///\return The point for the icon location
    PointPtr GetIconLocation( void );

    ///Get results data
    ///\return The ith result stored as a command
    //ves::open::xml::CommandPtr GetResult( int i );

    ///Get results data
    ///\return The ith result stored as a command
    ves::open::xml::CommandPtr GetResult( const std::string& inputName );
    
    ///Get results data
    ///\return The ith result stored as a command
    const std::vector< ves::open::xml::CommandPtr > GetResults();

    ///Get results data
    ///\return The number of results in this model
    //size_t GetNumberOfResults( void );

    ///Set the result for this models
    void SetResult( ves::open::xml::CommandPtr& input );

    ///Get input data
    ///\param i get the i'th input, to allocate a new DataValuePair pass in -1
    ///\return The ith input in this model
    //vesDEPRECATED( ves::open::xml::CommandPtr GetInput( int i ) );

    ///Get the input variable by name
    ///\param inputName Then name of the input to retrieve
    ///\return The input with the given name
    ves::open::xml::CommandPtr GetInput( const std::string& inputName );

    ///Get input data
    ///\return The ith result stored as a command
    const std::vector< ves::open::xml::CommandPtr > GetInputs();
    
    ///Allocate another input block for use
    ///\return The new input
    ///This function should be replaced with SetInput in users code since 
    ///ves is now using smart pointers
    //vesDEPRECATED( ves::open::xml::CommandPtr GetInput() );
    
    ///Set the input for this models
    void SetInput( ves::open::xml::CommandPtr& input );

    ///Get number of input data
    ///\return The total number of inputs
    //size_t GetNumberOfInputs( void );

    ///Get the i'th port for the model.
    ///\param i The i'th port you are after.
    ///\return The ith port selected by the user
    PortPtr GetPort( int i );

    ///Allocates a new port for the model.
    ///\return The new port
    PortPtr GetPort( void );

    ///Get port data
    size_t GetNumberOfPorts( void );

    ///Remove the i'th port for the model.
    ///\param i The i'th port you are after.
    void RemovePort( unsigned int i );

    ///Remove the selected port for the model.
    ///\param removePort The port you are after.
    void RemovePort( PortPtr removePort );

    ///Get the i'th information packet for a model.
    ///\param i The i'th packet you are after.
    ves::open::xml::ParameterBlockPtr GetInformationPacket( int i );

    ///Get the named information packet for a model.
    ///\param name The name of the packet you are after.
    ves::open::xml::ParameterBlockPtr GetInformationPacket( const std::string& name );

    ///Get info packets data
    size_t GetNumberOfInformationPackets( void );

    ///Get the geometry for the model.
    ves::open::xml::cad::CADNodePtr GetGeometry( void );

    ///Remove the i'th information packet for a model.
    ///\param i The i'th packet you are after.
    void RemoveInformationPacket( unsigned int i );

    ///Remove the named information packet from a model.
    ///\param name The name of the packet to remove.
    void RemoveInformationPacket( const std::string& name );

    ///Add a geometry node and return it
    /// if there is already geometry then the function will return that pointer
    void AddGeometry( const ves::open::xml::cad::CADAssemblyPtr& input );

    ///Add a geometry node and return it
    /// if there is already geometry then the function will return that pointer
    ves::open::xml::cad::CADNodePtr AddGeometry( void );
    
    ///Delete the geometry for this model
    void DeleteGeometry( void );

    ///Set the vendor name on the model
    void SetVendorName( const std::string& vendorName );

    ///Get the vendor name of the model
    const std::string& GetVendorName( void );

    ///Add a model attribute
    void SetModelAttribute( ves::open::xml::CommandPtr modelAttribute );

    ///Get model attributes
    ///\return The model attributes used by third party solvers
    ves::open::xml::CommandPtr GetModelAttribute( void );

    ///New interface for handling systems of systems
    ///Set the sub network for this model
    ///\param network The subnetwork for this model
    void SetSubSystem( ves::open::xml::model::SystemPtr inputSystem );

    ///Get the sub network for this model
    ///\return The subnetwork for this model
    ves::open::xml::model::SystemPtr GetSubSystem();

    ///Set the parent model pointer of this model
    void SetParentModel( ModelPtr parent );

    ///Get the parent model pointer of this class
    ///\return The pointer to the parent model, may be null
    ModelPtr GetParentModel();
    
    ///Print operator to help debug veopen issues
    friend std::ostream& operator<<( std::ostream& os, const ModelPtr model )
    {
        os << "***********(ves::open::xml::model::Model)***********" << std::endl
            << "Model Name = " << model->mModelName << std::endl
            << "The Unit Name = " << model->mVendorUnit << std::endl
            << "Unique Model ID (old) = " << model->mUniqueModelID << std::endl
            << "GUID = " << model->mUuid << std::endl
            << "Object Type = " << model->mObjectType << std::endl
            << "Object Namespace = " << model->mObjectNamespace << std::endl
            << "Number of Ports = " << model->mPorts.size() << std::endl
            << "Number of Results = " << model->mResults.size() << std::endl
            << "Number of Inputs = " << model->mInputs.size() << std::endl
            << "Number of Info Packets = " 
                <<  model->mInformationPackets.size() << std::endl
            << "Icon Scale = " << model->mIconScale << std::endl
            << "Icon Rotation = " << model->mIconRotation << std::endl
            << "Icons Hidden Flag = " << model->mIconHiddenFlag << std::endl
            << "Icon Mirror = " << model->mIconMirror << std::endl
            << "Model Inputs are: " << std::endl;
            
            for( std::map< std::string, ves::open::xml::CommandPtr >::iterator 
                iter = model->mInputs.begin(); 
                iter != model->mInputs.end(); ++iter )
            {
                os << (iter->second);
            }
        
            return os;
    }
    

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );

private:
    ///raw datatypes of Model that are specified in the verg_model.xsd file
    std::string     mModelName;///<The name of the model.
    unsigned int    mUniqueModelID;///<The unique model id.
    std::string     mIconFileName;///<The filename for the icon if any at all. Can possibly use an which would be compiled at runtime.

    std::vector< PortPtr >  mPorts;///<The vector port data if any for a model.
    PointPtr                mIconLocation;///<The icon location point container.
    ///The data value pair will contain all the results for a paticular model
    std::map< std::string, ves::open::xml::CommandPtr > mResults;///<The classes hold the results for the model.
    ///The data value pair will contain the model inputs for the model
    std::map< std::string, ves::open::xml::CommandPtr > mInputs;///<The classes hold the inputs for the model.
    ///The parameter block holds all the data the was formerly stored in the param file
    std::vector< ves::open::xml::ParameterBlockPtr > mInformationPackets;///<The classes hold relevant data to represent the model.

    ///The CADNode contains the tree structure for the geometry
    ves::open::xml::cad::CADAssemblyPtr mGeometry;///<The classes hold the geometry for the model.
    ///The vendor name for the model
    std::string mVendorUnit;///<The string that stores the unit name that this model maps to
    ves::open::xml::CommandPtr mModelAttribute;///<The structure that stores all of the model attributes
    float mIconScale;///<The icon scale to set the right size for conductor and xplorer
    float mIconRotation;///<The icon rotation for conductor and xplorer
    int mIconHiddenFlag;  //hides icon
    ///the icon image needs to be mirrored 1 = no, 2 = horizontally, 3= vertically
    unsigned int mIconMirror;
    ///The sub network for this model
    ves::open::xml::model::SystemPtr mSubSystem;
    ModelWeakPtr mParentModel;
};
}
}
}
}
#endif// MODEL_H_
