//--- Header Include ---//
#include "DataSetArray.h"

// --- C++ Includes --- //
#include <iostream>

// --- This plugin Includes --- //
#include "DataSet.h"

using namespace vascularsimviewer;


// Constructor
DataSetArray::DataSetArray( DataSet* parent )
    :
    mParent( parent ),
    status( false )
{
    ;
}

// Destructor
DataSetArray::~DataSetArray()
{
    ;
}

//////////////////////////
void DataSetArray::SetArrayName( const std::string& name )
{
    arrayName = name;
}

///////////////////////////
const std::string& DataSetArray::GetArrayName()
{
    return arrayName;
}

///////////////////////////
bool DataSetArray::GetArrayStatus()
{
    return status;
}

///////////////////////////
void DataSetArray::SetArrayStatus( bool inStatus )
{
    status = inStatus;
}
