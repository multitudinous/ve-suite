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
#ifndef CLONE_H
#define CLONE_H

/*!\file Clone.h
*/

/*!\class ves::xplorer::scenegraph::Clone
*
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class SceneNode;
}
}
}

// --- OSG Includes --- //
namespace osg
{
class Node;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS Clone
{
public:
    ///Base Constructor
    Clone();

    ///Constructor
    ///\param original The original node to be cloned
    Clone( osg::Node* original );

    //Destructor
    ~Clone();

    ///Clones a scenegraph node
    ///\param original The original node to be cloned
    void CloneNode( osg::Node* original );

    ///Set the translation array of the clone
    ///\param translation The translation array pointer
    void SetTranslationArray( double* translation );

    ///Set the rotation array of the clone
    ///\param rotation The rotation array pointer
    void SetRotationArray( double* rotation );

    ///Set the scale array of the clone
    ///\param scale The scale array pointer
    void SetScaleArray( double* scale );

    ///Return the cloned structure including the transform
    ves::xplorer::scenegraph::DCS* GetClonedGraph();

protected:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_cloneTransform;///<The cloned structure including the transform

};
}
}
}

#endif //CLONE_H
