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

#ifndef VES_XPLORER_SCENEGRAPH_PROJECTION_TECHNIQUE_H
#define VES_XPLORER_SCENEGRAPH_PROJECTION_TECHNIQUE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/technique/Technique.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace technique
{
/*!\file ProjectionTechnique.h
 * SelectTechnique API
 */

/*!\class ves::xplorer::scenegraph::technique::ProjectionTechnique
 *
 */
class VE_SCENEGRAPH_EXPORTS ProjectionTechnique : public Technique
{
public:
    ///
    ProjectionTechnique();

    ///
    virtual ~ProjectionTechnique();

    ///
    void SetAlpha( float const& alpha );

    ///
    void SetNearPlane( float const& nearPlane );

    ///
    void SetFarPlane( float const& farPlane );

    ///
    void SetFocalDistance( float const& focalDistance );

    ///
    void SetFocalRange( float const& focalRange );

    ///Set the picture frame bool
    void SetPictureFrame( bool pictureFrame = false );
    
protected:
    ///
    virtual void DefinePasses();

private:
    ///
    osg::ref_ptr< osg::Uniform > m_alpha;

    ///
    osg::ref_ptr< osg::Uniform > m_nearPlaneUniform;

    ///
    osg::ref_ptr< osg::Uniform > m_farPlaneUniform;

    ///
    osg::ref_ptr< osg::Uniform > m_focalDistanceUniform;

    ///
    osg::ref_ptr< osg::Uniform > m_focalRangeUniform;

    ///Control wether a red highlight is projected onto the geometry being 
    ///selected with the projection technique
    osg::ref_ptr< osg::Uniform > m_pictureFrameUniform;

};
} //end technique
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_PROJECTION_TECHNIQUE_H
