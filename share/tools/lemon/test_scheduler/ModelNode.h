#pragma once

#include <string>

namespace iaf
{
namespace scheduler
{
class ModelNode
{
public:
    ModelNode();
    ~ModelNode();
    
    void Preprocess();

    void RunModel();
        
    void Postprocess();

    void SetModelName( std::string const& name );
    
    std::string GetResults();
    
private:
    std::string m_modelName;
};
}
}
