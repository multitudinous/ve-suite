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

#ifndef VES_XPLORER_SCENEGRAPH_TECHNIQUE_PARALLAX_MAPPING_H
#define VES_XPLORER_SCENEGRAPH_TECHNIQUE_PARALLAX_MAPPING_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/technique/Technique.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Node;
class Texture2D;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace technique
{
/*!\file ParallaxMapping.h
 * ParallaxMapping API
 */

/*!\class ves::xplorer::scenegraph::technique::ParallaxMapping
 *
 */
class VE_SCENEGRAPH_EXPORTS ParallaxMapping : public Technique
{
public:
    ///Constructor
    ///\param stateSet
    ParallaxMapping( osg::Node* node );

    ///Destructor
    virtual ~ParallaxMapping();

    ///
    void BumpMapping();

    ///
    void ReliefMapping();

    ///
    void SetBaseMap( osg::Image* const image );

    ///
    void SetNormalMap( osg::Image* const image );

    ///
    void SetHeightMap( osg::Image* const image );

    ///
    void SilhouetteClipping();

protected:
    ///
    virtual void DefinePasses();

private:
    ///
    osg::Node* m_node;

    ///
    osg::ref_ptr< osg::StateSet > m_stateSet;

    ///
    osg::ref_ptr< osg::Texture2D > m_baseMap;

    ///
    osg::ref_ptr< osg::Texture2D > m_normalMap;

    ///
    osg::ref_ptr< osg::Texture2D > m_heightMap;

};

} //end technique
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_TECHNIQUE_PARALLAX_MAPPING_H
