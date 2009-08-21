#ifndef POINT_DATA_ARRAY_H
#define POINT_DATA_ARRAY_H

//--- wxWidgets Includes ---//
#include <wx/checkbox.h>

// --- C++ Includes --- ///
#include <string>

namespace vascularsimviewer
{
class DataSet;

class DataSetArray
{
    public:
        // Constructor
        DataSetArray( DataSet* parent );
        
        // Destructor
        ~DataSetArray();
        
        const std::string& GetArrayName();
        
        void SetArrayName( const std::string& name );
        
        bool GetArrayStatus();
        
        void SetArrayStatus( bool inStatus );
        
    private:
    
        std::string arrayName;
        
        bool status;
        
        DataSet* mParent;
};
}

#endif // POINT_DATA_ARRAY_H
