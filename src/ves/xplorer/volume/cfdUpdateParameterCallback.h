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
#ifndef CFD_UPDATE_PARAMETER_CALLBACK_H
#define CFD_UPDATE_PARAMETER_CALLBACK_H
/*!\file cfdUpdateParameterCallback.h
* cfdUpdateParameterCallback API
*/

/*!\class ves::xplorer::volume::cfdUpdateParameterCallback
*
*/
#ifdef _OSG

#include <osg/Uniform>
#include <ves/VEConfig.h>
namespace ves
{
namespace xplorer
{
namespace volume
{
   class VE_TEXTURE_BASED_EXPORTS cfdUpdateParameterCallback
      : public osg::Uniform::Callback{

      public:
	      cfdUpdateParameterCallback(); 
  
	/*cfdUpdateParameterCallback(const cfdUpdateParameterCallback &copy,
                           const osg::CopyOp &copyop = osg::CopyOp::SHALLOW_COPY);
*/
         enum cfdParameterType{VECTOR,MATRIX,TIME};
         enum cfdParameterSize{ONE=0,TWO,THREE,FOUR};
         virtual void operator () (osg::Uniform* uniVar, osg::NodeVisitor* nv);

         void setTypeAndSize(cfdParameterType type,
                           cfdParameterSize size)
         {
            _type = type;
            _size = size;
         }

         void updateParameter(float* value);
   
      protected:
         float _value[4];
         cfdParameterType _type;
         cfdParameterSize _size;
   };
}
}
}
#endif //_OSG
#endif //CFD_UPDATE_PARAMETER_CALLBACK_H
