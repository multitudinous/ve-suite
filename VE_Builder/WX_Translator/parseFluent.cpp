/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004, 
    6/10/2004 - fixed some typos
    
*/
#include "fluentIO.h"
#include "fluentCase.h"
#include <string>

namespace FluentReader {
void caseToVTK( Case *fcase, std::string filename );
void caseToVTK( Case *fcase, Case *fdata, std::string filename );
void caseToVTK( Case *fcase, Case *fdata, int var_id, std::string filename );
}

void parseSet( std::string casefile, std::string datafile, bool isBinary, bool isGzip, int var_id )
{
    std::string outfile = casefile + ".vtk";
    std::string check, info;

    check = casefile + ".check";
    info = casefile + ".info";
    FluentReader::FluentIO * infile1 = new FluentReader::FluentIO(
        casefile, info, check, isBinary, isGzip);
    /*  binary, gzip */

    FluentReader::Case *case1 = new FluentReader::Case(infile1);
    case1->read();
    delete(infile1);  
    case1->toGlobalFaceConnect();
    case1->toGlobalCellConnect();
    FluentReader::caseToVTK( case1, casefile + ".vtk" );
    //case1->textDump(casefile +".dump");    

    check = datafile + ".check";
    info = datafile + ".info";
    FluentReader::FluentIO * infile2 = new FluentReader::FluentIO(
        datafile, info, check, isBinary, isGzip);
    FluentReader::Case *data1 = new FluentReader::Case(infile2);
    data1->read();
    delete(infile2);
 
    //data1->textDump(datafile +".dump");    
    FluentReader::caseToVTK( case1, data1, var_id, datafile + ".vtk" );

    delete(case1);
    delete(data1);
    
}


void parse()
{
    FluentReader::FluentIO *infile;
    FluentReader::Case *caseFile;
 
    infile = new FluentReader::FluentIO(
        "cube_6.msh",
        "parse.info",
        "parse.check",
        false, false ) ; 

    caseFile = new FluentReader::Case(infile);
    caseFile->read();
    delete(infile);  
    caseFile->textDump("parse_localFace.dump");    
    caseFile->toGlobalFaceConnect();
    caseFile->textDump("parse_globalFace.dump");    
    caseFile->toGlobalCellConnect();
    caseFile->textDump("parse.dump");    
    FluentReader::caseToVTK( caseFile, "grid.vtk" );
    return;    
}

int main()
{
    parse();
    /* the varid has been set to 200 which is the mass fraction of 
    methane (the first species variable) at the current time step, if 
    you look at the .info file you will be able to generate files for
    other variables.  Also the files contain a blanking variable to
    tunoff parent cells on hierachical grids */
    /* it is unadvisable to do a textDump on the following datasets particularly
    the 3D file */
    parseSet("combustor2D.cas","combustor2D.dat", true, false, 200);
    parseSet("combustor3D.cas","combustor3D.dat", true, false, 200);
}

