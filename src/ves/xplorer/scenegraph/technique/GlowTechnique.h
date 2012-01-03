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

#ifndef GLOW_TECHNIQUE_H
#define GLOW_TECHNIQUE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/technique/Technique.h>

// --- STL Includes --- //
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace technique
{
/*!\file GlowTechnique.h
 * GlowTechnique API
 */

/*!\class ves::xplorer::scenegraph::technique::GlowTechnique
 *
 */
class VE_SCENEGRAPH_EXPORTS GlowTechnique : public Technique
{
public:
    ///Constructor
    ///\param stateSet
    GlowTechnique( osg::ref_ptr< osg::StateSet > stateSet );

    ///Destructor
    virtual ~GlowTechnique();

    ///
    void SetColor( osg::Vec3 const& glowColor );

protected:
    ///
    virtual void DefinePasses();

private:
    ///This is a copy of the stateset passed into the constructor =>
    ///so it should have its own memory location as to not affect the original
    osg::ref_ptr< osg::StateSet > m_stateSet;

    ///
    osg::ref_ptr< osg::Uniform > m_glowColor;

};

} //end technique
} //end scenegraph
} //end xplorer
} //end ves

#endif //GLOW_TECHNIQUE_H
