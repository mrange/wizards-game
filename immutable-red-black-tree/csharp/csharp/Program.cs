using System;
using System.Collections.Generic;
using System.Linq;

namespace csharp
{
    enum Color
    {
        Red     ,
        Black   ,
    }

    sealed class RedBlackTree<TKey, TValue>
        where TKey : IComparable<TKey>
    {
        public RedBlackTree (
            Color                        color  ,
            RedBlackTree<TKey, TValue>   left   ,
            RedBlackTree<TKey, TValue>   right  ,
            TKey                         key    ,
            TValue                       value  
            )
        {
            Color   = color  ;
            Left    = left   ;
            Right   = right  ;
            Key     = key    ;
            Value   = value  ;
        }

        public readonly Color                        Color   ;
        public readonly RedBlackTree<TKey, TValue>   Left    ;
        public readonly RedBlackTree<TKey, TValue>   Right   ;
        public readonly TKey                         Key     ;
        public readonly TValue                       Value   ;
    }

    static class RedBlackTree
    {
        static RedBlackTree<TKey, TValue> Recolor<TKey,TValue> (
            this RedBlackTree<TKey, TValue> tree    , 
            Color                           color   )
            where TKey : IComparable<TKey>
        {
            if (tree == null)
            {
                return null;
            }

            return new RedBlackTree<TKey,TValue> (
                        color       , 
                        tree.Left   , 
                        tree.Right  , 
                        tree.Key    , 
                        tree.Value  );
        }

        static RedBlackTree<TKey, TValue> Revalue<TKey,TValue> (
                this RedBlackTree<TKey, TValue> tree, TValue value  )
            where TKey : IComparable<TKey>
        {
            if (tree == null)
            {
                return null;
            }

            return new RedBlackTree<TKey,TValue> (
                        tree.Color      , 
                        tree.Left       , 
                        tree.Right      , 
                        tree.Key        , 
                        value           );
        }

        static RedBlackTree<TKey, TValue> Rebranch<TKey,TValue> (
                this RedBlackTree<TKey, TValue> tree    , 
                Color                           color   , 
                RedBlackTree<TKey, TValue>      left    , 
                RedBlackTree<TKey, TValue>      right   )
            where TKey : IComparable<TKey>
        {
            if (tree == null)
            {
                return null;
            }

            return new RedBlackTree<TKey,TValue> (
                        color       , 
                        left        , 
                        right       , 
                        tree.Key    , 
                        tree.Value  );
        }

        public static bool TryFind<TKey, TValue> (
                            this RedBlackTree<TKey, TValue>     tree    , 
                            TKey                                key     , 
                            out TValue                          value   )
            where TKey : IComparable<TKey>
        {
            value = default (TValue);

            if (tree == null)
            {
                return false;
            }

            var compareTo = key.CompareTo (tree.Key);

            if (compareTo == 0)
            {
                value = tree.Value;
                return true;
            }
            else if (compareTo < 0)
            {
                return tree.Left.TryFind (key, out value);
            }
            else
            {
                return tree.Right.TryFind (key, out value);
            }
        }

        static RedBlackTree<TKey, TValue> LeftBalance<TKey, TValue> (
                this RedBlackTree<TKey, TValue> tree)
            where TKey : IComparable<TKey>
        {
            if (!(tree != null && tree.Color == Color.Black))
            {
                return tree;
            }

            var left = tree.Left;
            if (!(left != null && left.Color == Color.Red))
            {
                return tree;
            }

            var left_left   = left.Left;
            var left_right  = left.Right;

            if (left_left != null && left_left.Color == Color.Red)
            {
                return left
                        .Rebranch(
                            Color.Red, 
                            left_left.Recolor (Color.Black), 
                            tree.Rebranch (Color.Black, left.Right, tree.Right)
                            );
            }
            else if (left_right != null && left_right.Color == Color.Red)
            {
                return left_right
                        .Rebranch(
                            Color.Red, 
                            left.Rebranch (Color.Black, left.Left, left_right.Left), 
                            tree.Rebranch (Color.Black, left_right.Right, tree.Right)
                            );
            }
            else
            {
                return tree;
            }
        }

        static RedBlackTree<TKey, TValue> RightBalance<TKey, TValue> (
                this RedBlackTree<TKey, TValue> tree)
            where TKey : IComparable<TKey>
        {
            if (!(tree != null && tree.Color == Color.Black))
            {
                return tree;
            }

            var right = tree.Right;
            if (!(right != null && right.Color == Color.Red))
            {
                return tree;
            }

            var right_left   = right.Left;
            var right_right  = right.Right;

            if (right_right != null && right_right.Color == Color.Red)
            {
                return right
                        .Rebranch(
                            Color.Red, 
                            tree.Rebranch (Color.Black, tree.Left, right.Left),
                            right_right.Recolor (Color.Black)
                            );
            }
            else if (right_left != null && right_left.Color == Color.Red)
            {
                return right_left
                        .Rebranch(
                            Color.Red, 
                            tree.Rebranch (Color.Black, tree.Left, right_left.Left),
                            right.Rebranch (Color.Black, right_left.Right, right.Right) 
                            );
            }
            else
            {
                return tree;
            }
        }

        static RedBlackTree<TKey, TValue> AddOrReplaceImpl<TKey, TValue> (
                this RedBlackTree<TKey, TValue> tree    , 
                TKey                            key     , 
                TValue                          value   )
            where TKey : IComparable<TKey>
        {
            if (tree == null)
            {
                return new RedBlackTree<TKey, TValue> (Color.Red, null, null, key, value);
            }

            var compareTo = key.CompareTo (tree.Key);

            if (compareTo == 0)
            {
                return tree.Revalue (value);
            }
            else if (compareTo < 0)
            {
                return tree
                    .Left
                    .AddOrReplaceImpl (key, value)
                    .LeftBalance ()
                    ;
            }
            else
            {
                return tree
                    .Right
                    .AddOrReplaceImpl (key, value)
                    .RightBalance ()
                    ;
            }
        }

        public static RedBlackTree<TKey, TValue> AddOrReplace<TKey, TValue> (
                        this RedBlackTree<TKey, TValue> tree    , 
                        TKey                            key     , 
                        TValue                          value   )
            where TKey : IComparable<TKey>
        {
            return tree
                .AddOrReplaceImpl (key, value)
                .Recolor (Color.Black)
                ;
        }

        public static RedBlackTree<TKey, TValue> ToRedBlackTree<TKey, TValue, T> (
                            this IEnumerable<T> values          , 
                            Func<T, TKey>       keySelector     ,
                            Func<T, TValue>     valueSelector   )
            where TKey : IComparable<TKey>
        {
            if (values == null)
            {
                return null;
            }

            RedBlackTree<TKey, TValue> result = null;

            foreach (var value in values)
            {
                result = result.AddOrReplace (keySelector (value), valueSelector (value));
            }

            return result;
        }

        public static RedBlackTree<TKey, TValue> ToRedBlackTree<TKey, TValue> (
                            this IEnumerable<TValue>    values          , 
                            Func<TValue, TKey>          keySelector     )
            where TKey : IComparable<TKey>
        {
            return values.ToRedBlackTree (keySelector, v => v);
        }

    }

    static class Program
    {
        static void Test (this IEnumerable<int> values, string testRun)
        {
            Console.WriteLine ("Running test run: {0}", testRun);

            var vs = (values ?? new int[0]).ToArray ();
            var uvs = values.Distinct ().ToArray ();

            var rbt = vs.ToRedBlackTree (v => v, v => 2*v);

            foreach (var v in vs)
            {
                int vv;
                var r = rbt.TryFind (v, out vv);
                if (!r)
                {
                    Console.WriteLine ("FAILED: {0} not found in tree", v);
                }
                else if (vv != v * 2)
                {
                    Console.WriteLine ("FAILED: {0} found in tree but wrong value {1}<>{2}", v, vv, 2*v);
                }
            }

        }

        static void Main(string[] args)
        {
            (new [] {31,41,59,26,53,58,97,93}).Test("Manual");
        }
    }
}
