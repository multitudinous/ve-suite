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
#ifndef OPACITY_VISITOR_H
#define OPACITY_VISITOR_H
/*!\file OpacityVisitor.h
OpacityVisitor API
*/
/*!\class OpacityVisitor
*
*/
#include <ves/VEConfig.h>

#include <osg/ref_ptr>
#include <osg/NodeVisitor>
namespace osg
{
    class StateSet;
}
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class VE_SCENEGRAPH_UTILS_EXPORTS OpacityVisitor : public osg::NodeVisitor
{
public:
    OpacityVisitor( osg::Node* osg_node, bool storeState, 
        bool state, float alpha=1.0 );
    virtual ~OpacityVisitor();

    virtual void apply( osg::Geode& node );
    virtual void apply( osg::Group& node );
    
    //setup the blending and renderbin details for a stateset
    //based on transparency
    ///\param stateset The stateset to modify
    void SetupBlendingForStateSet( osg::StateSet* stateset);
    

private:
    bool transparent;
    float m_alpha;
    ///If you want the visitor to attach the statesets to the drawables
    ///so that state can be saved after a file is loaded in
    bool mStoreState;
};
}
}
}
}

#endif //OPACITY_VISITOR_H
