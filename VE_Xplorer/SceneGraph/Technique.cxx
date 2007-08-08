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

// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/Technique.h"
#include "VE_Xplorer/SceneGraph/DCS.h"

// --- OSG Includes --- //
#include <osgUtil/CullVisitor>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Technique::Technique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
int Technique::GetNumPasses() const
{
    return static_cast< int >( m_passes.size() );
}
////////////////////////////////////////////////////////////////////////////////
osg::StateSet* Technique::GetPassStateSet( int i )
{
    return m_passes[ i ].get();
}
////////////////////////////////////////////////////////////////////////////////
const osg::StateSet* Technique::GetPassStateSet( int i ) const
{
    return m_passes[ i ].get();
}
////////////////////////////////////////////////////////////////////////////////
void Technique::Traverse( osg::NodeVisitor& nv, VE_SceneGraph::DCS* dcs )
{
    TraverseImplementation( nv, dcs );
}
////////////////////////////////////////////////////////////////////////////////
void Technique::DirtyPasses()
{
    m_passes.clear();
}
////////////////////////////////////////////////////////////////////////////////
void Technique::AddPass( osg::StateSet* ss )
{
    if( ss )
    {
        m_passes.push_back( ss );
        ss->setRenderBinDetails( static_cast< int >( m_passes.size() ), "RenderBin" );
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* Technique::GetOverrideChild( int )
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void Technique::TraverseImplementation( osg::NodeVisitor& nv, VE_SceneGraph::DCS* dcs )
{
    //Define passes if necessary
    if( m_passes.empty() )
    {
        DefinePasses( dcs );
    }

    //Special actions must be taken if the node visitor is actually a CullVisitor
    osgUtil::CullVisitor* cv = dynamic_cast< osgUtil::CullVisitor* >( &nv );

    //Traverse all passes
    for( int i = 0; i < GetNumPasses(); ++i )
    {
        //Push the i-th pass' StateSet if necessary
        if( cv )
        {
            cv->pushStateSet( m_passes[ i ].get() );
        }

        //Traverse the override node if defined
        //Otherwise traverse children as a Group would do
        osg::Node* override = GetOverrideChild( i );
        if( override )
        {
            override->accept( nv );
        }
        else
        {
            dcs->InheritedTraverse( nv );
        }

        //Pop the StateSet if necessary
        if( cv )
        {
            cv->popStateSet();
        }
    }        
}
////////////////////////////////////////////////////////////////////////////////
