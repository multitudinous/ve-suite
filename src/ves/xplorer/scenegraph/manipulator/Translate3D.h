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
 * Date modified: $Date: 2009-05-13 15:17:12 -0600 (Wed, 13 May 2009) $
 * Version:       $Rev: 12684 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: Translate3D.h 12684 2009-05-13 21:17:12Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef TRANSLATE_3D_H
#define TRANSLATE_3D_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/CompoundDragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class TranslateAxis;

/*!\file Translate3D.h
 * Translate1D API
 */

/*!\class ves::xplorer::scenegraph::Translate3D
 *
 */
class VE_SCENEGRAPH_EXPORTS Translate3D : public CompoundDragger
{
public:
    ///
    Translate3D();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Translate3D(
        const Translate3D& translate3D,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, Translate3D );

protected:
    ///
    virtual ~Translate3D();

    ///
    virtual void SetupDefaultGeometry();

private:
    osg::ref_ptr< TranslateAxis > m_xTranslateAxis;
    osg::ref_ptr< TranslateAxis > m_yTranslateAxis;
    osg::ref_ptr< TranslateAxis > m_zTranslateAxis;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //TRANSLATE_3D_H
