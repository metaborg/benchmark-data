package dynsem.strategies;

import org.spoofax.interpreter.terms.IStrategoTerm;
import org.spoofax.interpreter.terms.ITermFactory;
import org.strategoxt.lang.Context;

import trans.all_to_java_editor_0_0;
import trans.parse_file_0_0;

public class GenInterp {

	public static void main(String[] args) {

		Context ctx = trans.Main.init();
		ITermFactory tf = ctx.getFactory();

		IStrategoTerm ast = parse_file_0_0.instance.invoke(ctx,
				tf.makeString(args[0]));
		IStrategoTerm position = tf.makeList();
		IStrategoTerm path = tf.makeString(args[0]);
		IStrategoTerm projectpath = tf.makeString(args[1]);

		IStrategoTerm tup = tf.makeTuple(ast, position, ast, path, projectpath);

		all_to_java_editor_0_0.instance.invoke(ctx, tup);
	}

}
