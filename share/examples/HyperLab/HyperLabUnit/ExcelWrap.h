#ifndef EXCEL_WRAP_H
#define EXCEL_WRAP_H

class CApplication;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>
#include <fstream>

class ExcelWrap
{
public:
    ExcelWrap();
    ~ExcelWrap();
    void SetSensorData( CApplication* app );
    std::string GetSensorData();
private:
    std::vector< std::string > fileList;
    std::string currentFile;
    std::string sensorData;
};

#endif //EXCEL_WRAP_H