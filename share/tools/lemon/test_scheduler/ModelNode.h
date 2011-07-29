#pragma once

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

private:
    
};
}
}
