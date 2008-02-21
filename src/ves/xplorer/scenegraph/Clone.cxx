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
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/Group.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Geode>
#include <osg/CopyOp>
#include <osg/MatrixTransform>
#elif _OPENSG
#endif

// --- C/C++ Libraries --- //
#include <typeinfo>
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Clone::Clone()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Clone::Clone( osg::Node* original )
{
    CloneNode( original );
}
////////////////////////////////////////////////////////////////////////////////
Clone::~Clone()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Clone::CloneNode( osg::Node* original )
{
    if( !m_cloneTransform.valid() )
    {
        m_cloneTransform = new ves::xplorer::scenegraph::DCS();
    }

    //Deep copy nodes so that picking is accurate and so that physics will work properly in the future
    if( dynamic_cast< ves::xplorer::scenegraph::DCS* >( original ) )
    {
        m_cloneTransform = new ves::xplorer::scenegraph::DCS(
            *static_cast< ves::xplorer::scenegraph::DCS* >( original ),
            osg::CopyOp::DEEP_COPY_NODES | 
            osg::CopyOp::DEEP_COPY_STATESETS | 
            osg::CopyOp::DEEP_COPY_STATEATTRIBUTES );
    }
    else if( dynamic_cast< osg::Geode* >( original ) )
    {
        m_cloneTransform->addChild( new osg::Geode( *static_cast< osg::Geode* >( original ), 
           osg::CopyOp::DEEP_COPY_NODES | 
           osg::CopyOp::DEEP_COPY_STATESETS | 
           osg::CopyOp::DEEP_COPY_STATEATTRIBUTES ) );
    }
    else
    {
        std::cout << "ERROR : Cast not present " << std::endl;
        std::cout << typeid( *original ).name() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetTranslationArray( double* translation )
{
    if( m_cloneTransform.valid() )
    {
        m_cloneTransform->SetTranslationArray( translation );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetRotationArray( double* rotation )
{
    if( m_cloneTransform.valid() )
    {
        m_cloneTransform->SetRotationArray( rotation );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Clone::SetScaleArray( double* scale )
{
    if( m_cloneTransform.valid() )
    {
        m_cloneTransform->SetScaleArray( scale );
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* Clone::GetClonedGraph()
{
    if( m_cloneTransform.valid() )
    {
        return m_cloneTransform.get();
    }

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
