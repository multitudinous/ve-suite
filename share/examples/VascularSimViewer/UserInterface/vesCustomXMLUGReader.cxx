//--- Header Include ---//
#include "vesCustomXMLUGReader.h"

// --- vtk Includes --- //
#include <vtkXMLDataElement.h>

// --- C++ Include --- //
#include <iostream>

using namespace vascularsimviewer;

// Constructor
vesCustomXMLUGReader::vesCustomXMLUGReader()
{
    ;
}

// Desctructor
vesCustomXMLUGReader::~vesCustomXMLUGReader()
{
    ;
}

// Return an interger with number of point arrays
int vesCustomXMLUGReader::GetNumberofPointArrays()
{

    std::cout << PointDataElements[0]->GetParent()->GetParent()->GetName()  << std::endl;
    std::cout << PointDataElements[0]->GetParent()->GetName()  << std::endl;
    std::cout << PointDataElements[0]->GetName()  << std::endl;
    std::cout << PointDataElements[0]->GetNumberOfNestedElements() << std::endl;

    int i;
    int j;
    for( i=0; i<PointDataElements[0]->GetNumberOfNestedElements(); i++ )
    {
        std::cout << PointDataElements[0]->GetNestedElement(i)->GetName() << std::endl;
        
        for( j=0; j< PointDataElements[0]->GetNestedElement(i)->GetNumberOfAttributes(); j++ )
        {
            std::string name = PointDataElements[0]->GetNestedElement(i)->GetAttributeName(j);
            std::string attribute = PointDataElements[0]->GetNestedElement(i)->GetAttribute( name.c_str() );
            
            std::cout << "   " << name <<"= " << attribute << std::endl;
        }
    }
}