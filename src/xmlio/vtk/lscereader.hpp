#ifndef __XMLIO_LSCE_READER__
#define __XMLIO_LSCE_READER__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "inetcdf4.hpp"

/// VTK headers ///
#include "vtkDataObjectAlgorithm.h"

#include "vtkStdString.h"

#include "vtkUnstructuredGrid.h"
#include "vtkStructuredGrid.h"
#include "vtkRectilinearGrid.h"

#include "vtkSmartPointer.h"

#include "vtkStringArray.h"
#include "vtkFloatArray.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkCellArray.h"

#include "vtkCellData.h"
#include "vtkPointData.h"

namespace xmlioserver
{
   namespace vtk
   {
      /// ////////////////////// Déclarations ////////////////////// ///

      class VTK_EXPORT vtkLSCEReader
         : public vtkDataObjectAlgorithm
      {
         public :

         //-------------------------------------------------------------
            typedef enum _gridtype
            { RECTILINEAR = 0, CURVILINEAR, UNSTRUCTURED } GridType;
         //-------------------------------------------------------------

            /// Spécifique VTK ///
            static vtkLSCEReader * New(void);
            vtkTypeMacro(vtkLSCEReader, vtkDataObjectAlgorithm);
            void PrintSelf(ostream& os, vtkIndent indent);

         //-------------------------------------------------------------

            /// Mutateurs ///
            void SetFileName(const StdString & fileName);

            void AddVariableToSelection(const StdString & varName);
            void RemoveSelectedVariable(const StdString & varName);
            void RemoveAllSelectedVariables(void);

            void SetGridType (GridType type);

            void AcceptTemporalOnly(bool value);
            void Accept3DOnly(bool value);
            void AcceptCellOnly(bool value);

         //-------------------------------------------------------------

            /// Accesseurs ///
            const StdString & GetFileName(void) const;

            const std::set<StdString> & GetSelectedVariables(void) const;

         //-------------------------------------------------------------

            /// Tests ///
            bool IsUnstructured(void) const;
            bool IsCurvilinear(void) const;
            bool IsRectilinear(void) const;

            bool HasSelectedVariable(void) const;

            /// Visualisation ///
            static void ShowVariable(const vtkStdString & filename,
                                     const vtkStdString & varname);

         protected:

            /// Constructeur ///
            vtkLSCEReader(void);

            /// Destructeur ///
            virtual ~vtkLSCEReader(void);

         //-------------------------------------------------------------
            void GetSpacings(const vtkStdString & coordinate,
                             bool bounds, vtkFloatArray * spacing);

         //-------------------------------------------------------------

            void CreateRectilinearGrid(vtkRectilinearGrid *, vtkInformation *,
                                        vtkFloatArray *, vtkFloatArray *, vtkFloatArray *,
                                        vtkIntArray *);

            void CreateStructuredGrid(vtkStructuredGrid *, vtkInformation *,
                                      vtkPoints *, vtkIntArray *);

            void CreateUnstructuredGrid(vtkUnstructuredGrid *, vtkInformation *,
                                        vtkPoints *, vtkCellArray *, int);

         //-------------------------------------------------------------

            void CreateSimpleGrid (int xi, int xf, int yi, int yf, int zi, int zf,
                                   vtkFloatArray *, vtkFloatArray *, vtkFloatArray *,
                                   vtkIntArray *);

            void CreateSimpleGrid (int xi, int xf, int yi, int yf, int zi, int zf,
                                   vtkPoints *, vtkIntArray *);

            void CreateSimpleGrid (int xi, int xf, int yi, int yf, int zi, int zf,
                                   vtkPoints *, vtkCellArray *, vtkIntArray *);

         //-------------------------------------------------------------

            void GetRectilinearConnectivity(int, int, int, vtkCellArray *);

         //-------------------------------------------------------------

            void AddPoint(vtkPoints * points, float *  value, bool proj);

            void GetPoints(const vtkStdString & xcoordinate,
                           const vtkStdString & ycoordinate,
                           const vtkStdString & zcoordinate,
                           bool bounds, bool proj,
                           vtkPoints * points, vtkIntArray * dimensions);

         //-------------------------------------------------------------

            void GetCellsAndPoints(const vtkStdString & xcoordinate,
                                   const vtkStdString & ycoordinate,
                                   const vtkStdString & zcoordinate,
                                   bool bounds, bool proj, StdSize nbvertex,
                                   vtkCellArray * cells, vtkPoints * points,
                                   vtkIntArray * dimensions);

         //-------------------------------------------------------------

            void AddScalarData(vtkDataSet * output, const StdString & varname,
                               StdSize record, bool bounds);

         //-------------------------------------------------------------

            /// Traitements ///
            virtual int RequestData
               (vtkInformation *, vtkInformationVector **, vtkInformationVector *);

            //virtual int RequestInformation
            //   (vtkInformation *, vtkInformationVector **, vtkInformationVector *);

            //virtual int RequestDataObject
            //   (vtkInformation *, vtkInformationVector **, vtkInformationVector *);

            virtual int ProcessRequest
               (vtkInformation *, vtkInformationVector **, vtkInformationVector *);

         //-------------------------------------------------------------

         private:

            /// Constructeurs ///
            vtkLSCEReader(const vtkLSCEReader &);        // Not implemented
            vtkLSCEReader(const vtkLSCEReader * const);  // Not implemented

            /// Opérateur ///
            void operator=(const vtkLSCEReader &);       // Not implemented

         //-------------------------------------------------------------

            /// Propriétés privées ///
            StdString FileName;

            GridType CurGridType;
            std::set<StdString> VarNames;
            bool A3D, ATemporal, ACell;

            boost::shared_ptr<io::CINetCDF4> Reader;

      }; // class vtkLSCEReader

      ///----------------------------------------------------------------

   } // namespace vtk
} // namespace xmlioserver

#endif //__XMLIO_LSCE_READER__
