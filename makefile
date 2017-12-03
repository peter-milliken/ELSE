#
# Create a tar package for upload to the Emacs Package Library
#

src = else-mode.el else-template.el else-structs.el else-template-mode.el \
      else-lexer.el else.texi else.info else.pdf else-mode-pkg.el \
      Template.lse Template-cust.lse dir

.PHONY : HelpMessage

HelpMessage:
	@echo ""
	@echo ""
	@echo "Usage:"
	@echo "       make VERS=\"2.0.0\" final"
	@echo ""
	@echo ""

final : else-mode-$(VERS).tar

doc : else.pdf else.info dir

else.pdf : else.texi
	tex else.texi
	texindex else.??
	tex else.texi
	dvipdfm else

else.info : else.texi
	makeinfo else.texi

dir : else.info
	install-info else.info dir

else-mode-$(VERS).tar : $(src)
	@rm -rf else-mode-$(VERS)
	@mkdir else-mode-$(VERS)
	for x in $(src); do \
	  cp $$x else-mode-$(VERS); \
	done
	tar -cvf else-mode-$(VERS).tar else-mode-$(VERS)


