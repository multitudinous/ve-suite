/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef _VES_XPLORER_SCENEGRAPH_META_NODE_H
#define _VES_XPLORER_SCENEGRAPH_META_NODE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <boost/any.hpp>
#include <boost/shared_ptr.hpp>

// --- OSG Includes --- //

#include <osgDB/ReadFile>

#include <osg/Referenced>

#include <map>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{


/*!\file MetaNode.h
*/

/*!\class ves::xplorer::scenegraph::MetaNode
*
*/

/*!\namespace ves::xplorer::scenegraph
*
*/
class VE_SCENEGRAPH_EXPORTS MetaNode : public osg::Object
{
protected:
    typedef std::pair<const std::string, const boost::any> ResourcePair;
    typedef std::map<const std::string, const boost::any> ResourceMap;
    typedef ResourceMap::const_iterator ResourceMapConstIterator;
    typedef ResourceMap::iterator ResourceMapIterator;

public:
    ///Base Constructor
    MetaNode();
    MetaNode( const MetaNode& rhs, osg::CopyOp copyop=osg::CopyOp::SHALLOW_COPY )
        : 
        osg::Object( rhs )
    {
        *this = rhs;
    }
    
    MetaNode& operator=( const MetaNode& rhs ){
        return *this;
    }

    META_Object(ves::xplorer::scenegraph,MetaNode);

    /* Looks up resource and creates new if not available yet! */
    /*template<typename T, template< typename > class Container >
    const Container<T> get( const std::string& resourceName)
    {
        ResourceMapIterator iter = mResourceMap.find( resourceName );
        if( iter != mResourceMap.end() )
        {
            return boost::any_cast<Container<T> >(iter->second);
        }
        // Was not found. So, lets make it!
        Container<T> real_val = createResource<T, Container>( resourceName );
        boost::any to_append = real_val;
        mResourceMap.insert( ResourcePair( resourceName, real_val) );
        return real_val;
    }
    */
    template<class T >
    T get( const std::string& resourceName) const
    {
        ResourceMapConstIterator iter = mResourceMap.find( resourceName );
        if( iter != mResourceMap.end() )
        {
            T tempVar = boost::any_cast<T>(iter->second);
            return tempVar;
        }
        // Was not found. So, lets make it!
        /*T real_val = createResource<T>( resourceName );
        boost::any to_append = real_val;
        mResourceMap.insert( ResourcePair( resourceName, real_val) );
        return real_val;*/
    }
    
    /* Explicitly add a resource. */
    void add( const std::string& resourceName, const boost::any& value );

    /* Explicitly remove a resource. */
    bool remove( const std::string& resourceName );

    /*///
    ///\return
    virtual const char* className() const;
    
    ///
    ///\param copyop
    ///\return
    virtual osg::Object* clone( const osg::CopyOp& copyop ) const;
    
    ///
    ///\return
    virtual osg::Object* cloneType() const;
    
    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;
    */
protected:
    /*template<typename T, template< typename > class Container >
    Container<T> createResource( const std::string& resourceName)
    {
        //return Container<T>( new T( resourceName ) );
        return Container<T>( new T( ) );
    }*/

private:
    ///Destructor
    virtual ~MetaNode();

    ResourceMap mResourceMap;
};
/*
template<>
osg::ref_ptr< osg::Image > ResourceManager::createResource( const std::string& resourceName )
{
   osg::ref_ptr< osg::Image > ret_val( osgDB::readImageFile( resourceName ) );
   return ret_val;
}
*/
}
}
}

#endif // _VES_XPLORER_SCENEGRAPH_META_NODE_H
