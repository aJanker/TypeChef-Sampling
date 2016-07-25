package de.fosd.typechef

import java.io.{File, FileInputStream, ObjectInputStream, ObjectStreamClass}
import java.util

import scala.collection.JavaConverters._
import de.fosd.typechef.FamilyBasedVsSampleBased.SimpleConfiguration
import de.fosd.typechef.featureexpr.{FeatureExprFactory, SingleFeatureExpr}
import de.fosd.typechef.options.OptionException
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c.{CTypeContext, TranslationUnit, _}

object Sampling extends EnforceTreeHelper {
    def main(args: Array[String]) {
        // load options
        val opt = new FamilyBasedVsSampleBasedOptions()
        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException => if (!opt.isPrintVersion) throw o
            }

            if (opt.isPrintVersion) {
                var version = "development build"
                try {
                    val cl = Class.forName("de.fosd.typechef.Version")
                    version = "version " + cl.newInstance().asInstanceOf[VersionInfo].getVersion
                } catch {
                    case e: ClassNotFoundException =>
                }

                println("TypeChef " + version)
                return
            }
        }

        catch {
            case o: OptionException =>
                println("I ain't serializing shit:")
                val result = FamilyBasedVsSampleBased.loadSerializedTasks_flo(new File(".")).head._2
                println(result.head)
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                return
        }

        processFile(opt)
    }

    def processFile(opt: FamilyBasedVsSampleBasedOptions) {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val smallFM = opt.getSmallFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setSmallFeatureModel(smallFM)
        val fullFM = opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFullFeatureModel(fullFM) // should probably be fixed in how options are read
        if (!opt.getFilePresenceCondition.isSatisfiable(fullFM)) {
            println("file has contradictory presence condition. existing.")
            return
        }

        var ast: TranslationUnit = null
        if (opt.reuseAST && opt.parse && new File(opt.getSerializedTUnitFilename).exists()) {
            println("#loading AST.")
            try {
                ast = Frontend.loadSerializedAST(opt.getSerializedTUnitFilename)
            } catch {
                case e: Throwable => println(e.getMessage); ast=null
            }
            if (ast == null)
                println("#... failed reading AST\n")
        } else {
            println("#... failed parsing AST\n")
        }

        ast = prepareAST[TranslationUnit](ast)

        if (ast != null) {
            FamilyBasedVsSampleBased.checkErrorsAgainstProducts(fullFM, fullFM, ast, opt, "[PRODUCT] ")
            FamilyBasedVsSampleBased.checkErrorsAgainstSamplingConfigs(fullFM, fullFM, ast, opt, "[CONFIG] ")
            // FamilyBasedVsSampleBased.typecheckProducts(fullFM, fullFM, ast, opt, "")
        }
    }

    def lex(opt: FamilyBasedVsSampleBasedOptions): TokenReader[CToken, CTypeContext] = {
        val tokens = new lexer.LexerFrontend().run(opt, opt.parse)
        val in = CLexerAdapter.prepareTokens(tokens)
        in
    }
}
