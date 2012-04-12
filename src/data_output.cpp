#include "data_output.hpp"

#include "attribute_template_impl.hpp"
#include "group_template_impl.hpp"
#include "context.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CDataOutput::~CDataOutput(void)
      { /* Ne rien faire de plus */ }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid
         (const boost::shared_ptr<CGrid> grid)
      {
         if (grid->domain_ref.isEmpty())
            ERROR("CDataOutput::writeGrid(grid)",
                   << " domain is not defined !");

         if (grid->axis_ref.isEmpty())
         {
            this->writeGrid(CDomain::get(grid->domain_ref.getValue()));
         }
         else
         {
            this->writeGrid(CDomain::get(grid->domain_ref.getValue()),
                            CAxis::get(grid->axis_ref.getValue()));
         }
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFile
         (const boost::shared_ptr<CFile>  file)
      {
         this->writeFile_(file);
      }
 
      void CDataOutput::syncFile(void)
      {
         this->syncFile_();
      }

      void CDataOutput::closeFile(void)
      {
         this->closeFile_();
      }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid
         (const boost::shared_ptr<CDomain> domain,
          const boost::shared_ptr<CAxis> axis)
      {
         this->writeDomain_(domain);
         this->writeAxis_(axis);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid
         (const boost::shared_ptr<CDomain> domain)
      {
         this->writeDomain_(domain);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeField
         (const boost::shared_ptr<CField> field)
      {
         boost::shared_ptr<CContext> context = CContext::getCurrent() ;
         boost::shared_ptr<CCalendar> calendar = context->getCalendar();
         
         this->writeField_(field);
         this->writeTimeAxis_(field, calendar);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFieldGrid
         (const boost::shared_ptr<CField> field)
      {
         this->writeGrid(field->getRelGrid());
      }
      
      //----------------------------------------------------------------
      
      void CDataOutput::writeFieldData(const boost::shared_ptr<CField> field)
      {
         boost::shared_ptr<CGrid> grid = CGrid::get(field->grid_ref.getValue());
         boost::shared_ptr<CDomain> domain = CDomain::get(grid->domain_ref.getValue());
            
//         if (domain->isEmpty()) return;
         this->writeFieldData_(field);
      }
      
      ///----------------------------------------------------------------

} // namespace xios
