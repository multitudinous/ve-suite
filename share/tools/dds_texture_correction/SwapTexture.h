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
#ifndef TEXTURE_REPLACE_VISITOR_H
#define TEXTURE_REPLACE_VISITOR_H
/*!\file SwapTexture.h
 *   SwapTexture API
 *!\class SwapTexture
 *
 */
#include <ves/VEConfig.h>

#include <osg/ref_ptr>
#include <osg/NodeVisitor>
//VE_SCENEGRAPH_UTILS_EXPORTS
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class  SwapTexture : public osg::NodeVisitor
{
public:
    SwapTexture( osg::Node* osg_node, bool writeDDSFile = false );
    virtual ~SwapTexture();

    virtual void apply( osg::Node& node );
    virtual void apply( osg::Geode& node );

private:
    void CheckStateSet( osg::StateSet* stateSet );
    bool m_writeDDSFile;
};
}
}
}
}

#endif //MATERIAL_INITIALIZER_H
