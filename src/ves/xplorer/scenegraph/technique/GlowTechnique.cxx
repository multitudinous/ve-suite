/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/scenegraph/technique/GlowTechnique.h>

// --- OSG Includes --- //
#include <osg/Depth>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace technique
{

////////////////////////////////////////////////////////////////////////////////
GlowTechnique::GlowTechnique( osg::ref_ptr< osg::StateSet > stateSet )
    :
    Technique(),
    m_stateSet( stateSet ),
    //Nice colors
    //GreenYellow( 0.57255, 1.0, 0.34118, 1.0 );
    //GreenBlue( 0.34118, 1.0, 0.57255, 1.0 );
    //Blue( 0.34118, 0.57255, 1.0, 1.0 );
    m_glowColor(
        new osg::Uniform( "glowColor", osg::Vec3( 1.0, 0.0, 1.0 ) ) )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
GlowTechnique::~GlowTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GlowTechnique::DefinePasses()
{
    //Pass 1
    {
        m_stateSet->addUniform( m_glowColor.get() );

        AddPass( m_stateSet.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void GlowTechnique::SetColor( osg::Vec3 const& glowColor )
{
    m_glowColor->set( glowColor );
}
////////////////////////////////////////////////////////////////////////////////

} //end technique
} //end scenegraph
} //end xplorer
} //end ves
