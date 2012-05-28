/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/model/System.h>

#include <boost/concept_check.hpp>

#include <sstream>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::cad;
using namespace ves::open::xml::model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Model::Model()
    :
    XMLObject(),
    boost::enable_shared_from_this<Model>(),
    mUniqueModelID( 0 ),
    mGeometry( CADAssemblyPtr() ),
    mModelAttribute( CommandPtr() ),
    mIconScale( 1.0f ),
    mIconRotation( 0.0f ),
    mIconHiddenFlag( 0 ),
    mIconMirror( 0 ),
    mParentModel( ModelPtr() ),
    m_parentSystem( SystemPtr() )
{
    mIconLocation = PointPtr( new Point() );
    SetObjectType( "Model" );
    SetObjectNamespace( "Model" );
}
///////////////////////////////////
Model::~Model()
{
    if( mSubSystem )
    {
        mSubSystem->SetParentModel( ModelPtr() );
    }
    mSubSystem = SystemPtr();
    m_parentSystem = SystemPtr();

    mPorts.clear();

    mResults.clear();

    mInputs.clear();

    mInformationPackets.clear();
}
///////////////////////////////////////////
Model::Model( const Model& input )
    :
    XMLObject( input ),
    boost::enable_shared_from_this<Model>( input )
{
    mPluginName = input.mPluginName;
    mPluginType = input.mPluginType;
    mUniqueModelID = input.mUniqueModelID;
    mIconFileName = input.mIconFileName;
    mVendorUnit = input.mVendorUnit;
    mIconScale = input.mIconScale;
    mIconRotation = input.mIconRotation;
    mIconMirror = input.mIconMirror;
    mIconHiddenFlag = input.mIconHiddenFlag;

    mSubSystem = SystemPtr();
    if( input.mSubSystem )
    {
        mSubSystem = SystemPtr( input.mSubSystem );
    }

    m_parentSystem = input.m_parentSystem;

    mPorts.clear();
    std::copy( input.mPorts.begin(),
               input.mPorts.end(),
               std::back_inserter( mPorts ) );

    mIconLocation =  input.mIconLocation;

    mResults.clear();
    mResults = input.mResults;
    //std::copy( input.mResults.begin(),
    //           input.mResults.end(),
    //           mResults );

    mInputs.clear();
    mInputs = input.mInputs;
    //std::copy( input.mInputs.begin(),
    //           input.mInputs.end(),
    //           mInputs );


    mInformationPackets.clear();
    std::copy( input.mInformationPackets.begin(),
               input.mInformationPackets.end(),
               std::back_inserter( mInformationPackets ) );

    mGeometry = ves::open::xml::cad::CADAssemblyPtr();
    if( input.mGeometry )
    {
        mGeometry = input.mGeometry;
    }

    mModelAttribute = CommandPtr();
    if( input.mModelAttribute )
    {
        mModelAttribute = input.mModelAttribute;
    }

    //add ptr to parent - used for querying
    mParentModel = input.mParentModel;
}
/////////////////////////////////////////////////////
Model& Model::operator=( const Model& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mPluginName = input.mPluginName;
        mPluginType = input.mPluginType;
        mUniqueModelID = input.mUniqueModelID;
        mIconFileName = input.mIconFileName;
        mVendorUnit = input.mVendorUnit;
        mIconScale = input.mIconScale;
        mIconRotation = input.mIconRotation;
        mIconMirror = input.mIconMirror;
        mIconHiddenFlag = input.mIconHiddenFlag;

        mSubSystem = SystemPtr();
        if( input.mSubSystem )
        {
            mSubSystem = SystemPtr( input.mSubSystem );
        }

        m_parentSystem = input.m_parentSystem;

        mPorts.clear();
        std::copy( input.mPorts.begin(),
                   input.mPorts.end(),
                   std::back_inserter( mPorts ) );

        mIconLocation =  input.mIconLocation;

        mResults.clear();
        mResults = input.mResults;
        //std::copy( input.mResults.begin(),
        //           input.mResults.end(),
        //           mResults );

        mInputs.clear();
        mInputs = input.mInputs;
        //std::copy( input.mInputs.begin(),
        //           input.mInputs.end(),
        //           mInputs );


        mInformationPackets.clear();
        std::copy( input.mInformationPackets.begin(),
                   input.mInformationPackets.end(),
                   std::back_inserter( mInformationPackets ) );

        mGeometry = ves::open::xml::cad::CADAssemblyPtr();
        if( input.mGeometry )
        {
            mGeometry = input.mGeometry;
        }

        mModelAttribute = CommandPtr();
        if( input.mModelAttribute )
        {
            mModelAttribute = input.mModelAttribute;
        }

        //add ptr to parent - used for querying
        mParentModel = input.mParentModel;

    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
/*std::ostream& operator<<( std::ostream& os, const Model& model )
{
    os << "Model Name = " << model.mPluginName << std::endl
        << "The Unit Name = " << model->mVendorUnit << std::endl
        << "Unique Model ID (old) = " << model->mUniqueModelID << std::endl
        << "GUID = " << model->mUuid << std::endl
        << "Object Type = " << model->mObjectType << std::endl
        << "Object Namespace = " << model->mObjectNamespace << std::endl
        << "Number of Ports = " << model->mPorts.size() << std::endl
        << "Number of Results = " << model->mResults.size() << std::endl
        << "Number of Inputs = " << model->mInputs.size() << std::endl
        << "Number of Info Packets = " <<  model->mInformationPackets.size() << std::endl
        << "Icon Scale = " << model->mIconScale << std::endl
        << "Icon Rotation = " << model->mIconRotation << std::endl
        << "Icons Hidden Flag = " << model->mIconHiddenFlag << std::endl
        << "Icon Mirror = " << model->mIconMirror << std::endl;
    return os;
}*/
////////////////////////////////////////////////////////////////////////////////
void Model::SetPluginName( const std::string& name )
{
    mPluginName = name;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetPluginType( const std::string& type )
{
    mPluginType = type;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetModelID( unsigned int id )
{
    mUniqueModelID = id;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconFilename( const std::string& filename )
{
    mIconFileName = filename;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetObjectFromXMLData( DOMNode* element )
{
    DOMElement* currentElement = 0;
    if( element->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( element );
    }

    if( !currentElement )
    {
        return;
    }

    //Setup uuid for model element
    {
        std::string tempUuid;
        ves::open::xml::XMLObject::GetAttribute( currentElement, "id", tempUuid );
        if( !tempUuid.empty() )
        {
            mUuid = tempUuid;
        }
    }
    //get variables by tags
    DOMElement* dataValueStringName = 0;

    {
        dataValueStringName = GetSubElement( currentElement, "name", 0 );
        if( dataValueStringName )
        {
            GetDataFromElement( dataValueStringName, mPluginName );
            dataValueStringName = 0;
        }
        else
        {
            GetAttribute( currentElement, "name", mPluginName );
        }
    }

    {
        GetAttribute( currentElement, "plugintype", mPluginType );

        if( mPluginType.empty() )
        {
            //This is a new addition so some ves files may not have a plugin
            //name specified to use. Therefore use the previous method
            //which was the model name.
            mPluginType = mPluginName;
        }
    }

    GetAttribute( currentElement, "vendorUnit", mVendorUnit );

    {
        dataValueStringName = GetSubElement( currentElement, "ID", 0 );
        if( dataValueStringName )
        {
            GetDataFromElement( dataValueStringName,  mUniqueModelID );
        }
        else
        {
            std::string idString;
            GetAttribute( currentElement, "ID", idString );
            std::istringstream inputStream( idString );
            inputStream >> mUniqueModelID;
        }
    }

    {
        dataValueStringName = GetSubElement( currentElement, "icon", 0 );
        if( dataValueStringName )
        {
            GetDataFromElement( dataValueStringName, mIconFileName );
        }
        else
        {
            mIconFileName = std::string( "no_icon" );
        }
        GetAttribute( dataValueStringName, "iconScale", mIconScale );
        GetAttribute( dataValueStringName, "iconRotation", mIconRotation );
        GetAttribute( dataValueStringName, "iconMirror", mIconMirror );
        GetAttribute( dataValueStringName, "iconHidden", mIconHiddenFlag );
    }

    {
        dataValueStringName = GetSubElement( currentElement, "iconLocation", 0 );

        mIconLocation = PointPtr( new Point() );
        mIconLocation->SetObjectFromXMLData( dataValueStringName );
    }

    //get the geometry nodes
    {
        DOMNodeList* childNodes = currentElement->getElementsByTagName(
                                      Convert( "geometry" ).toXMLString() );
        if( childNodes->getLength() > 0 )
        {
            DOMElement* child =
                static_cast< DOMElement* >( childNodes->item( 0 ) );
            //dataValueStringName = GetSubElement( currentElement, "geometry", 0 );
            if( child->getParentNode() == currentElement )
            {
                mGeometry = CADAssemblyPtr( new CADAssembly( "oops" ) );
                mGeometry->SetObjectFromXMLData( child );
            }
        }
    }
    //Get ports
    {
        DOMNodeList* childNodes = currentElement->getElementsByTagName(
                                      Convert( "ports" ).toXMLString() );
        unsigned int numberOfPortData = childNodes->getLength();
        for( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            DOMElement* child =
                static_cast< DOMElement* >( childNodes->item( i ) );
            if( child->getParentNode() == currentElement )
            {
                //dataValueStringName = GetSubElement( currentElement, "ports", i );
                mPorts.push_back( PortPtr( new Port() ) );
                mPorts.back()->SetObjectFromXMLData( child );
            }
            else
            {
                break;
            }
        }
    }
    //Get Results
    {
        DOMNodeList* childNodes = currentElement->getElementsByTagName(
                                      Convert( "results" ).toXMLString() );
        unsigned int numberOfPortData = childNodes->getLength();
        for( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            DOMElement* child =
                static_cast< DOMElement* >( childNodes->item( i ) );
            if( child->getParentNode() == currentElement )
            {
                //dataValueStringName = GetSubElement( currentElement, "results", i );
                CommandPtr tempCommand( new Command() );
                tempCommand->SetObjectFromXMLData( child );
                mResults[ tempCommand->GetCommandName() ] = tempCommand;
            }
            else
            {
                break;
            }
        }
    }
    //Get Inputs
    {
        DOMNodeList* childNodes = currentElement->getElementsByTagName(
                                      Convert( "inputs" ).toXMLString() );
        unsigned int numberOfPortData = childNodes->getLength();
        for( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            DOMElement* child =
                static_cast< DOMElement* >( childNodes->item( i ) );
            if( child->getParentNode() == currentElement )
            {
                //dataValueStringName = GetSubElement( currentElement, "inputs", i );
                CommandPtr tempCommand( new Command() );
                tempCommand->SetObjectFromXMLData( child );
                mInputs[ tempCommand->GetCommandName() ] = tempCommand;
            }
            else
            {
                break;
            }
        }
    }
    //Get Information packets
    {
        DOMNodeList* childNodes = currentElement->getElementsByTagName(
                                      Convert( "informationPackets" ).toXMLString() );
        unsigned int numberOfPortData = childNodes->getLength();
        for( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            DOMElement* child =
                static_cast< DOMElement* >( childNodes->item( i ) );
            if( child->getParentNode() == currentElement )
            {
                //dataValueStringName = GetSubElement( currentElement, "informationPackets", i );
                mInformationPackets.push_back( ParameterBlockPtr( new ParameterBlock() ) );
                mInformationPackets.back()->SetObjectFromXMLData( child );
            }
            else
            {
                break;
            }
        }
    }

    //get the model attribute nodes
    {
        DOMNodeList* childNodes = currentElement->getElementsByTagName(
                                      Convert( "modelAttributes" ).toXMLString() );
        if( childNodes->getLength() > 0 )
        {
            DOMElement* child =
                static_cast< DOMElement* >( childNodes->item( 0 ) );
            if( child->getParentNode() == currentElement )
            {
                //dataValueStringName = GetSubElement( currentElement, "modelAttributes", 0 );
                mModelAttribute = CommandPtr( new Command() );
                mModelAttribute->SetObjectFromXMLData( child );
            }
        }
    }
    //Get the subSystem for this model
    {
        //dataValueStringName = GetSubElement( currentElement, "modelSubSystem", 0 );
        DOMNodeList* childNodes = currentElement->getElementsByTagName(
                                      Convert( "modelSubSystem" ).toXMLString() );
        if( childNodes->getLength() > 0 )
        {
            DOMElement* child =
                static_cast< DOMElement* >( childNodes->item( 0 ) );
            if( child->getParentNode() == currentElement )
            {
                mSubSystem = SystemPtr( new System() );
                //set parent
                mSubSystem->SetParentModel( shared_from_this() );
                mSubSystem->SetObjectFromXMLData( child );
            }
        }
    }
}
////////////////////////////////////////////////////////////
const std::string& Model::GetPluginName( void )
{
    return mPluginName;
}
////////////////////////////////////////////////////////////
const std::string& Model::GetPluginType( void )
{
    return mPluginType;
}
////////////////////////////////////////////////////////////
unsigned int Model::GetModelID( void )
{
    return mUniqueModelID;
}
////////////////////////////////////////////////////////////
const std::string& Model::GetIconFilename( void )
{
    return mIconFileName;
}
////////////////////////////////////////////////////////////
PointPtr Model::GetIconLocation( void )
{
    return mIconLocation;
}
////////////////////////////////////////////////////////////
/*CommandPtr Model::GetResult( int i )
{
    try
    {
        return mResults.at( i );
    }
    catch ( ... )
    {
        if( i >= 0 )
        {
            std::cerr << "The element request is out of sequence."
                << " Please ask for a lower number point."
                << " Returning NULL result command" << std::endl;
            return CommandPtr();
        }
    }
}*/
////////////////////////////////////////////////////////////
/*size_t Model::GetNumberOfResults( void )
{
    return mResults.size();
}*/
////////////////////////////////////////////////
CommandPtr Model::GetResult( const std::string& inputName )
{
    std::map< std::string, CommandPtr >::iterator iter =
        mResults.find( inputName );
    if( iter != mResults.end() )
    {
        return iter->second;
    }

    return CommandPtr();
}
////////////////////////////////////////////////
CommandPtr Model::GetInput( const std::string& inputName )
{
    std::map< std::string, CommandPtr >::iterator iter =
        mInputs.find( inputName );
    if( iter != mInputs.end() )
    {
        return iter->second;
    }

    return CommandPtr();
}
////////////////////////////////////////////////////////////
/*CommandPtr Model::GetInput( int i )
{
    try
    {
        return mInputs.at( i );
    }
    catch ( ... )
    {
        if( i >= 0 )
        {
            std::cerr << " Model::GetInput The element request is out of sequence."
            << " Please ask for a lower number point or -1 to request new element." << std::endl;
        }
        mInputs.push_back( CommandPtr(new Command() ) );
        return mInputs.back();
    }
}*/
////////////////////////////////////////////////////////////
/*ves::open::xml::CommandPtr Model::GetInput( void )
{
    mInputs.push_back( CommandPtr( new Command() ) );
    return mInputs.back();
}*/
////////////////////////////////////////////////////////////
/*size_t Model::GetNumberOfInputs( void )
{
    return mInputs.size();
}*/
////////////////////////////////////////////////////////////
PortPtr Model::GetPort( int i )
{
    try
    {
        return mPorts.at( i );
    }
    catch( ... )
    {
        //if ( i > (mPorts.size() + 1) )
        if( i >= 0 )
        {
            std::cerr << "The element request is out of sequence."
                      << " Please ask for a lower number point." << std::endl;
            return PortPtr();
        }
        else
        {
            mPorts.push_back( PortPtr( new Port( ) ) );
            return mPorts.back();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
PortPtr Model::GetPort( void )
{
    mPorts.push_back( PortPtr( new Port() ) );
    return mPorts.back();
}
////////////////////////////////////////////////////////////////////////////////
size_t Model::GetNumberOfPorts( void )
{
    return mPorts.size();
}
////////////////////////////////////////////////////////////////////////////////
void Model::RemovePort( unsigned int i )
{
    size_t count = 0;
    for( std::vector< PortPtr >::iterator iter = mPorts.begin();
            iter != mPorts.end(); ++iter )
    {
        if( count == i )
        {
            mPorts.erase( iter );
            break;
        }
        ++count;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::RemovePort( ves::open::xml::model::PortPtr removePort )
{
    for( std::vector< PortPtr >::iterator iter = mPorts.begin();
            iter != mPorts.end(); ++iter )
    {
        if( *iter == removePort )
        {
            mPorts.erase( iter );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
/*////////////////////////////////////////////////////////////
Port* Model::GetOutputPort( unsigned int i )
{
   try
   {
      return outputPorts.at( i );
   }
   catch (...)
   {
      if(i > ( outputPorts.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         mPorts.push_back( PortPtr( new Port() ) );
         outputPorts.push_back( mPorts.back() );
         return mPorts.back();
      }
   }
}
////////////////////////////////////////////////////////////
size_t Model::GetNumberOfOutputPorts( void )
{
   return outputPorts.size();
}*/
//////////////////////////////////////////////////////////////////////
ParameterBlockPtr Model::GetInformationPacket( const std::string& name )
{

    for( std::vector<ParameterBlockPtr>::iterator iter = mInformationPackets.begin();
            iter != mInformationPackets.end(); ++iter )
    {
        if( ( *iter )->GetName() == name )
        {
            return ( *iter );
        }
    }
    //std::cout<<"Parameter Block: "<<name<<std::endl;
    //std::cout<<"not found in Model: "<<uuid<<std::endl;
    return ParameterBlockPtr();
}
////////////////////////////////////////////////////////////
ParameterBlockPtr Model::GetInformationPacket( int i )
{
    try
    {
        return mInformationPackets.at( i );
    }
    catch( ... )
    {
        if( i >= 0 )
        {
            std::cerr << "The element request is out of sequence."
                      << " Please ask for a lower number point." << std::endl;
            return ParameterBlockPtr();
        }
        else
        {
            mInformationPackets.push_back(
                ParameterBlockPtr( new ParameterBlock( ) ) );

            return mInformationPackets.back();
        }
    }
}
////////////////////////////////////////////////////////////
size_t Model::GetNumberOfInformationPackets( void )
{
    return mInformationPackets.size();
}
////////////////////////////////////////////////////////////
CADNodePtr Model::GetGeometry( void )
{
    return mGeometry;
}
////////////////////////////////////////////////////////////
CADNodePtr Model::AddGeometry( void )
{
    if( mGeometry == 0 )
    {
        mGeometry = CADAssemblyPtr( new CADAssembly( "Model_Geometry" ) );
    }

    return mGeometry;
}
////////////////////////////////////////////////////////////
void Model::AddGeometry( const ves::open::xml::cad::CADAssemblyPtr& input )
{
    mGeometry = input;
}
////////////////////////////////////////////////////////////
void Model::DeleteGeometry( void )
{
    if( mGeometry )
    {
        mGeometry = CADAssemblyPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::RemoveInformationPacket( unsigned int i )
{
    std::vector< ves::open::xml::ParameterBlockPtr >::iterator iter;
    for( iter = mInformationPackets.begin(); iter != mInformationPackets.end(); ++iter )
    {
        if( mInformationPackets.at( i ) == ( *iter ) )
        {
            mInformationPackets.erase( iter );
            return;
        }
    }
}
//////////////////////////////////////////////////////
void Model::RemoveInformationPacket( const std::string& name )
{
    for( std::vector<ves::open::xml::ParameterBlockPtr>::iterator iter = mInformationPackets.begin();
            iter != mInformationPackets.end(); ++iter )
    {
        if( ( *iter )->GetName() == name )
        {
            mInformationPackets.erase( iter );
            return;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::_updateVEElement( const std::string& input )
{
    boost::ignore_unused_variable_warning( input );
    // write all the elements according to verg_model.xsd
    SetAttribute( "name", mPluginName );

    if( mPluginType.empty() )
    {
        mPluginType = mPluginName;
    }
    SetAttribute( "plugintype", mPluginType );
    SetAttribute( "id", mUuid );

    if( mVendorUnit.empty() )
    {
        mVendorUnit = mPluginName;
    }

    SetAttribute( "vendorUnit", mVendorUnit );
    //SetSubElement( "name", mPluginName );
    std::ostringstream dirStringStream;
    dirStringStream << mUniqueModelID;
    SetAttribute( "ID", dirStringStream.str() );
    //SetSubElement( "ID", mUniqueModelID );
    DOMElement* iconElement = SetSubElement( "icon", mIconFileName );
    ///
    {
        std::stringstream int2string;
        int2string << mIconScale;
        iconElement->setAttribute( Convert( "iconScale" ).toXMLString(),
                                   Convert( int2string.str() ).toXMLString() );
    }
    ///
    {
        std::stringstream int2string;
        int2string << mIconRotation;
        iconElement->setAttribute( Convert( "iconRotation" ).toXMLString(),
                                   Convert( int2string.str() ).toXMLString() );
    }
    ///
    {
        std::stringstream int2string;
        int2string << mIconMirror;
        iconElement->setAttribute( Convert( "iconMirror" ).toXMLString(),
                                   Convert( int2string.str() ).toXMLString() );
    }

    ///
    {
        std::stringstream int2string;
        int2string << mIconHiddenFlag;
        iconElement->setAttribute( Convert( "iconHidden" ).toXMLString(),
                                   Convert( int2string.str() ).toXMLString() );
    }

    SetSubElement<ves::open::xml::XMLObjectPtr>( "iconLocation", mIconLocation );
    SetSubElements( "ports", mPorts );

    std::vector< CommandPtr > tempResults;
    for( std::map< std::string, CommandPtr >::iterator
            iter = mResults.begin(); iter != mResults.end(); ++iter )
    {
        tempResults.push_back( iter->second );
    }
    SetSubElements( "results", tempResults );

    std::vector< CommandPtr > tempInputs;
    for( std::map< std::string, CommandPtr >::iterator
            iter = mInputs.begin(); iter != mInputs.end(); ++iter )
    {
        tempInputs.push_back( iter->second );
    }
    SetSubElements( "inputs", tempInputs );

    SetSubElements( "informationPackets", mInformationPackets );

    if( mGeometry )
    {
        SetSubElement<ves::open::xml::XMLObjectPtr>( "geometry", mGeometry );
    }

    if( mModelAttribute )
    {
        SetSubElement<ves::open::xml::XMLObjectPtr>( "modelAttributes", mModelAttribute );
    }

    if( mSubSystem )
    {
        SetSubElement<ves::open::xml::XMLObjectPtr>( "modelSubSystem", mSubSystem );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetVendorName( const std::string& vendorName )
{
    mVendorUnit = vendorName;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& Model::GetVendorName( void )
{
    return mVendorUnit;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetModelAttribute( CommandPtr modelAttribute )
{
    mModelAttribute = modelAttribute;
}
////////////////////////////////////////////////////////////////////////////////
CommandPtr Model::GetModelAttribute( void )
{
    return mModelAttribute;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconRotation( float rotation )
{
    mIconRotation = rotation;
}
////////////////////////////////////////////////////////////////////////////////
float Model::GetIconRotation( void )
{
    return mIconRotation;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconScale( float scale )
{
    mIconScale = scale;
}
////////////////////////////////////////////////////////////////////////////////
float Model::GetIconScale( void )
{
    return mIconScale;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconMirror( int mirror )
{
    mIconMirror = mirror;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconHiddenFlag( int flag )
{
    mIconHiddenFlag = flag;
}
////////////////////////////////////////////////////////////////////////////////
int Model::GetIconHiddenFlag( void )
{
    return mIconHiddenFlag;
}
////////////////////////////////////////////////////////////////////////////////
int Model::GetIconMirror( void )
{
    return mIconMirror;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetSubSystem( SystemPtr inputSystem )
{
    mSubSystem = inputSystem;
}
////////////////////////////////////////////////////////////////////////////////
SystemPtr Model::GetSubSystem()
{
    return mSubSystem;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetParentModel( ModelPtr parent )
{
    mParentModel = parent;
}
////////////////////////////////////////////////////////////////////////////////
ModelPtr Model::GetParentModel()
{
    return mParentModel.lock();
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetInput( ves::open::xml::CommandPtr& input )
{
    mInputs[ input->GetCommandName() ] = input;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetResult( ves::open::xml::CommandPtr& input )
{
    mResults[ input->GetCommandName() ] = input;
}
////////////////////////////////////////////////////////////////////////////////
const std::vector< CommandPtr > Model::GetResults()
{
    std::vector< CommandPtr > tempResults;
    for( std::map< std::string, CommandPtr >::iterator
            iter = mResults.begin(); iter != mResults.end(); ++iter )
    {
        tempResults.push_back( iter->second );
    }
    return tempResults;
}
////////////////////////////////////////////////////////////////////////////////
const std::vector< CommandPtr > Model::GetInputs()
{
    std::vector< CommandPtr > tempResults;
    for( std::map< std::string, CommandPtr >::iterator
            iter = mInputs.begin(); iter != mInputs.end(); ++iter )
    {
        tempResults.push_back( iter->second );
    }
    return tempResults;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetParentSystem( SystemPtr system )
{
    m_parentSystem = system;
}
////////////////////////////////////////////////////////////////////////////////
SystemPtr Model::GetParentSystem()
{
    return m_parentSystem;
}
////////////////////////////////////////////////////////////////////////////////
