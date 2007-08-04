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
 * Date modified: $Date: 2007-06-15 11:02:33 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8205 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: SelectTechnique.h 8205 2007-06-15 16:02:33Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef SELECT_TECHNIQUE_H
#define SELECT_TECHNIQUE_H

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/Technique.h"


namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS SelectTechnique : public Technique
{
public:
    SelectTechnique();
    ~SelectTechnique();
  
protected:
    virtual void DefinePasses();

private:

};
}

#endif //SELECT_TECHNIQUE_H
