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
 * Author:        $Author: mikelem $
 * Id:            $Id: cfdVTKFileHandler.h 8206 2007-06-15 16:06:13Z mikelem $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef COMPUTE_VECTOR_MAGNITUDE_RANGE_CALLBACK 
#define  COMPUTE_VECTOR_MAGNITUDE_RANGE_CALLBACK
/*!\file ComputeMeanCellBBoxLengthCallback.h
ComputeMeanCellBBoxLengthCallback API.
*/

/*!\class VE_Util::ComputeMeanCellBBoxLengthCallback
*
*/
class vtkDataSet;

#include "VE_Installer/include/VEConfig.h"
#include "VE_Xplorer/Utilities/DataObjectHandler.h"


namespace VE_Util
{
class VE_UTIL_EXPORTS ComputeVectorMagnitudeRangeCallback:
        public DataObjectHandler::DatasetOperatorCallback
{
public:    
    ///Constructor
    ComputeVectorMagnitudeRangeCallback();
    ///Destructor
    virtual ~ComputeVectorMagnitudeRangeCallback(){};
    ///The operation to do on each vtkDataSet in the vtkDataObject
    ///\param dataset The vtkDataSet to operate on
    virtual void OperateOnDataset(vtkDataSet* dataset);
    
    ///Get the Mean Cell BBox length
	///\param vMagRange The min and max magnitude of the active vector
    void GetVectorMagnitudeRange(double*& vMagRange);
protected:
    double m_magnitudeRange[2];///<The mean cell bbox length;
};
}
#endif

