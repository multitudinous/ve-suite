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
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/CreationEventHandler.h>

#include <utility>
#include <string>

using namespace ves::open::xml;

//utility to properly initialize and delete the Singleton XMLObjectFactory
namespace ves
{
namespace open
{
namespace xml
{
class ObjectFactoryMaker
{
public:
    ObjectFactoryMaker()
    {
        XMLObjectFactory::Instance();
    };
    ~ObjectFactoryMaker()
    {
        XMLObjectFactory::DeleteInstance();
    };
};
}
}
}

static ObjectFactoryMaker ObjectFactoryManager;

XMLObjectFactory* XMLObjectFactory::mInstanceOfFactory = 0;
//std::map<std::string,CreationEventHandler*> XMLObjectFactory::mObjectCreators;
/////////////////////////////////////
XMLObjectFactory::XMLObjectFactory( )
{}
/////////////////////////////////////
XMLObjectFactory::~XMLObjectFactory()
{
    if( mObjectCreators.size() )
    {
        for( std::map<std::string, CreationEventHandler* >::iterator itr = mObjectCreators.begin();
                itr != mObjectCreators.end(); itr++ )
        {
            delete itr->second;
            itr->second = 0;
        }
        mObjectCreators.clear();
    }
}
///////////////////////////////////////
void XMLObjectFactory::DeleteInstance()
{
    if( mInstanceOfFactory )
    {
        delete mInstanceOfFactory;
        mInstanceOfFactory = 0;
    }
}
//////////////////////////////////////////////
XMLObjectFactory* XMLObjectFactory::Instance()
{
    if( !mInstanceOfFactory )
    {
        mInstanceOfFactory = new XMLObjectFactory();
    }
    return mInstanceOfFactory;
}
/////////////////////////////////////////////////////////////////////////////////////
XMLObjectPtr XMLObjectFactory::CreateXMLObject( const std::string& objectType,
                                              std::string objectNameSpace )
{
    std::map<std::string, CreationEventHandler* >::iterator xmlCreator;
    //xmlCreator = mObjectCreators.find(objectNameSpace);
    for( xmlCreator = mObjectCreators.begin(); xmlCreator != mObjectCreators.end(); ++xmlCreator )
    {
        XMLObjectPtr temp = xmlCreator->second->CreateNewXMLObject( objectType );
        if( temp )
        {
            return temp;
        }
    }
    return XMLObjectPtr();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
XMLObjectPtr XMLObjectFactory::CreateXMLObjectCopy( XMLObjectPtr objectToCopy )
{
    std::string objectType = objectToCopy->GetObjectType();
    std::string objectNamespace = objectToCopy->GetObjectNamespace();

    ///I don't think we need this as we already have an object of the type we are creating
    ///therefore no need to check this.
    ///registration of the creator is taken care of in the constructor of the xmlobject -- mccdo
    //std::map<std::string,CreationEventHandler* >::iterator xmlCreator;
    //xmlCreator = mObjectCreators.find(objectNamespace);
    //if(xmlCreator != mObjectCreators.end())
    {
        return mObjectCreators[ objectNamespace ]->CreateNewXMLObjectCopy( objectType, objectToCopy );
    }
    //return 0;
}
/////////////////////////////////////////////////////////////////////////////
bool XMLObjectFactory::ObjectCreatorIsRegistered( const std::string& objectNamespace )
{
    std::map<std::string, CreationEventHandler* >::iterator xmlCreator;
    if( mObjectCreators.find( objectNamespace ) != mObjectCreators.end() )
    {
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
bool XMLObjectFactory::RegisterObjectCreator( const std::string& objectNamespace, CreationEventHandler* newCreator )
{
    std::map<std::string, CreationEventHandler* >::iterator xmlCreator;
    if( mObjectCreators.find( objectNamespace ) != mObjectCreators.end() )
    {
        return false;
    }
    if( objectNamespace != std::string( "XML" ) &&
            objectNamespace != std::string( "CAD" ) &&
            objectNamespace != std::string( "Model" ) &&
            objectNamespace != std::string( "Shader" ) )
    {
        std::cout << "Invalid namespace specified: " << objectNamespace << std::endl;
        std::cout << "Valid namespaces are: " << std::endl;
        std::cout << "XML" << std::endl;
        std::cout << "CAD" << std::endl;
        std::cout << "Shader" << std::endl;
        return false;
    }
    mObjectCreators[objectNamespace] = newCreator;
    return true;
}
