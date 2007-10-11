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
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef DATA_OBJECT_HANDLER_H
#define DATA_OBJECT_HANDLER_H
/*!\file DataObjectHandler.h
DataObjectHandler API. At a minimum, converts cell data to point data in each dataset of
the data object.
*/

/*!\class VE_Util::DataObjectHandler
*
*/
class vtkDataObject;
class vtkDataSet;

#include <ves/VEConfig.h>


namespace VE_Util
{
class VE_UTIL_EXPORTS DataObjectHandler
{
public:
    ///Constructor
    DataObjectHandler();
    ///Destructor
    virtual ~DataObjectHandler();

    /*!\class VE_Util::DataObjectHandler::DatasetOperatorCallback
    *
    */
    class DatasetOperatorCallback
    {
    public:    
        ///Constructor
        DatasetOperatorCallback(){};
	    ///Destructor
        virtual ~DatasetOperatorCallback(){};
	    ///The operation to do on each vtkDataSet in the vtkDataObject
	    ///\param dataset The vtkDataSet to operate on
	    virtual void OperateOnDataset(vtkDataSet* dataset) = 0;
    protected:
    };
    ///Do the operation specified in the DataObjectHandlerCallback\n
    ///on a vtkDataObject
    void OperateOnAllDatasetsInObject(vtkDataObject* dataObject);
    ///Set the operation to perform on each dataset in the vtkDataObject
	///Caller is responsible for cleaning up DatasetOperatorCallback memory
    ///\param dsoCbk The DataSetOperatorCallback
    void SetDatasetOperatorCallback(DatasetOperatorCallback* dsoCbk);

    ///Get the number of cell or point data arrays
    ///\param isPointData Return point or cell data array count
    unsigned int GetNumberOfDataArrays(bool isPointData=true);
protected:
    ///Convert the cell data to point data if it exists
    ///\param dataset The vtkDataSet
    void _convertCellDataToPointData(vtkDataSet* dataset);

    ///<Number of point data arrays
    unsigned int m_numberOfPointDataArrays;
    ///<Number of cell data arrays
    unsigned int m_numberOfCellDataArrays;
    DatasetOperatorCallback* m_datasetOperator;///<The DatasetOperatorCallback.
};
}
#endif
