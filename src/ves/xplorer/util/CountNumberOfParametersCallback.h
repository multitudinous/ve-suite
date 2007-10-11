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
 * Date modified: $Date: 2007-06-15 11:06:13 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8206 $
 * Author:        $Author: biv $
 * Id:            $Id: ComputeDataObjectBoundsCallback.h 9200 2007-10-03 19:17:07Z biv $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef COUNT_NUMBER_OF_PARAMETERS_CALLBACK 
#define  COUNT_NUMBER_OF_PARAMETERS_CALLBACK
/*!\file CountNumberOfParametersCallback.h
CountNumberOfParametersCallback API.
*/

/*!\class VE_Util::CountNumberOfParametersCallback
*
*/
class vtkDataSet;

#include <ves/VEConfig.h>
#include <ves/xplorer/utilDataObjectHandler.h>

#include <vector>
#include <string>
namespace VE_Util
{
class VE_UTIL_EXPORTS CountNumberOfParametersCallback:
        public DataObjectHandler::DatasetOperatorCallback
{
public:    
    ///Constructor
    CountNumberOfParametersCallback();
    ///Destructor
    virtual ~CountNumberOfParametersCallback();
    ///The operation to do on each vtkDataSet in the vtkDataObject
    ///\param dataset The vtkDataSet to operate on
    virtual void OperateOnDataset(vtkDataSet* dataset);
    
    ///Get the number of vectors or scalars
    ///\param  isVector The determining which type to count 
    unsigned int GetNumberOfParameters(bool isVector=false);

    ///Get the names of the scalars and vectors
    ///\param  isVector The determining which type to count 
    std::vector<std::string> GetParameterNames(bool isVector=false);
protected:
    unsigned int m_numberOfParameters[2];///<The number of parameters in the vtkDataObject;
	std::vector<std::string> m_scalarNames;///<The scalar names
	std::vector<std::string> m_vectorNames;///<The vector names
};
}
#endif

