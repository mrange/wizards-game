using System;
using System.Collections.Generic;
using System.Linq;

namespace csharp
{
    public class RedBlackTree<TKey, TValue> : IEnumerable<KeyValuePair<TKey, TValue>>
        where TKey : IComparable<TKey>
    {
        enum Color
        {
            Red     ,
            Black   ,
        }

        sealed class Node
        {
            public Node (
                Color   color  ,
                Node    left   ,
                Node    right  ,
                TKey    key    ,
                TValue  value  
                )
            {
                Color   = color  ;
                Left    = left   ;
                Right   = right  ;
                Key     = key    ;
                Value   = value  ;
            }

            public static readonly Node Empty = new Node (Color.Black, null, null, default (TKey), default (TValue));

            public Node Recolor (Color color)
            {
                return new Node (
                            color   , 
                            Left    , 
                            Right   , 
                            Key     , 
                            Value   );
            }

            public Node Revalue (TValue value)
            {
                return new Node (
                            Color   , 
                            Left    , 
                            Right   , 
                            Key     , 
                            value   );
            }

            public Node LeftRebranch (Node left)
            {
                return new Node (
                            Color   , 
                            left    , 
                            Right   , 
                            Key     , 
                            Value   );
            }

            public Node RightRebranch (Node right)
            {
                return new Node (
                            Color   , 
                            Left    , 
                            right   , 
                            Key     , 
                            Value   );
            }

            public Node Rebranch (Color color, Node left, Node right)
            {
                return new Node (
                            color   , 
                            left    , 
                            right   , 
                            Key     , 
                            Value   );
            }

            public bool IsEmpty
            {
                get
                {
                    return ReferenceEquals (this, Empty);
                }
            }

            public int Count
            {
                get
                {
                    if (IsEmpty)
                    {
                        return 0;
                    }

                    return 1 + Left.Count + Right.Count; 
                }
            }

            public int Depth
            {
                get
                {
                    if (IsEmpty)
                    {
                        return 0;
                    }

                    return 1 + Math.Max (Left.Depth, Right.Depth); 
                }
            }

            public bool TryFind (
                            TKey        key     , 
                            out TValue  value   )
            {
                if (IsEmpty)
                {
                    value = default (TValue);
                    return false;
                }

                var compareTo = key.CompareTo (Key);
                if (compareTo == 0)
                {
                    value = Value;
                    return true;
                }
                else if (compareTo < 0)
                {
                    return Left.TryFind (key, out value);
                }
                else
                {
                    return Right.TryFind (key, out value);
                }
            }

            Node LeftBalance ()
            {
                if ( !(!IsEmpty && Color == Color.Black))
                {
                    return this;
                }

                var left = Left;
                if ( !(!left.IsEmpty && left.Color == Color.Red))
                {
                    return this;
                }

                var left_left   = left.Left;
                var left_right  = left.Right;

                if (!left_left.IsEmpty && left_left.Color == Color.Red)
                {
                    return left
                            .Rebranch (
                                Color.Red, 
                                left_left.Recolor (Color.Black), 
                                Rebranch (Color.Black, left.Right, Right)
                                );
                }
                else if (!left_right.IsEmpty && left_right.Color == Color.Red)
                {
                    return left_right
                            .Rebranch (
                                Color.Red, 
                                left.Rebranch (Color.Black, left.Left, left_right.Left), 
                                Rebranch (Color.Black, left_right.Right, Right)
                                );
                }
                else
                {
                    return this;
                }
            }

            Node RightBalance ()
            {
                if ( !(!IsEmpty && Color == Color.Black))
                {
                    return this;
                }

                var right = Right;
                if ( !(!right.IsEmpty && right.Color == Color.Red))
                {
                    return this;
                }

                var right_left   = right.Left;
                var right_right  = right.Right;

                if (!right_right.IsEmpty && right_right.Color == Color.Red)
                {
                    return right
                            .Rebranch (
                                Color.Red, 
                                Rebranch (Color.Black, Left, right.Left),
                                right_right.Recolor (Color.Black)
                                );
                }
                else if (!right_left.IsEmpty && right_left.Color == Color.Red)
                {
                    return right_left
                            .Rebranch (
                                Color.Red, 
                                Rebranch (Color.Black, Left, right_left.Left),
                                right.Rebranch (Color.Black, right_left.Right, right.Right) 
                                );
                }
                else
                {
                    return this;
                }
            }

            public Node AddOrReplace (
                            TKey    key     , 
                            TValue  value   )
            {
                if (IsEmpty)
                {
                    return new Node (Color.Red, Empty, Empty, key, value);
                }

                var compareTo = key.CompareTo (Key);

                if (compareTo == 0)
                {
                    return Revalue (value);
                }
                else if (compareTo < 0)
                {
                    return LeftRebranch (                        
                                Left
                                .AddOrReplace (key, value)
                            ).LeftBalance ();
                }
                else
                {
                    return RightRebranch (                        
                                Right
                                .AddOrReplace (key, value)
                            ).RightBalance ();
                }
            }

            public readonly Color   Color   ;
            public readonly Node    Left    ;
            public readonly Node    Right   ;
            public readonly TKey    Key     ;
            public readonly TValue  Value   ;
        }

        RedBlackTree (Node node)
        {
            m_root = node ?? Node.Empty;
        }

        RedBlackTree ()
            : this (null)
        {
        }

        public readonly static RedBlackTree<TKey, TValue> Empty = new RedBlackTree<TKey, TValue> ();

        public bool TryFind (
                        TKey        key     , 
                        out TValue  value   )
        {
            return m_root.TryFind (key, out value);
        }

        public RedBlackTree<TKey, TValue> AddOrReplace (
                                            TKey    key     , 
                                            TValue  value   )
        {
            return new RedBlackTree<TKey, TValue> (m_root.AddOrReplace (key, value).Recolor (Color.Black));
        }

        public int Count
        {
            get
            {
                return m_root.Count;
            }
        }

        public int Depth
        {
            get
            {
                return m_root.Depth;
            }
        }

        readonly Node m_root;


        enum EnumeratorState
        {
            GoLeft      ,
            GoRight     ,
            Done        ,
        }

        struct EnumeratorStackFrame
        {
            public EnumeratorStackFrame (
                    Node             node   , 
                    EnumeratorState  state  )
            {
                Node    = node  ;
                State   = state ;
            }

            public readonly Node             Node    ;
            public readonly EnumeratorState  State   ; 
        }

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator ()
        {
            var stackFrames = new Stack<EnumeratorStackFrame> ();
            stackFrames.Push (new EnumeratorStackFrame (m_root, EnumeratorState.GoLeft));

            while (stackFrames.Count > 0)
            {
                var sf = stackFrames.Pop ();
                var node = sf.Node;
                if (node.IsEmpty)
                {
                    continue;
                }

                switch (sf.State)
                {   
                    case EnumeratorState.GoLeft:
                        stackFrames.Push (new EnumeratorStackFrame (node, EnumeratorState.GoRight));
                        stackFrames.Push (new EnumeratorStackFrame (node.Left, EnumeratorState.GoLeft));
                        break;
                    case EnumeratorState.GoRight:
                        yield return new KeyValuePair<TKey, TValue>(node.Key, node.Value);
                        stackFrames.Push (new EnumeratorStackFrame (node, EnumeratorState.Done));
                        stackFrames.Push (new EnumeratorStackFrame (node.Right, EnumeratorState.GoLeft));
                        break;
                    case EnumeratorState.Done:
                    default:
                        break;
                }
            }

        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator ()
        {
            return GetEnumerator ();
        }
    }

    static class RedBlackTree
    {
        public static RedBlackTree<TKey, TValue> ToRedBlackTree<TKey, TValue, T> (
                            this IEnumerable<T> values          , 
                            Func<T, TKey>       keySelector     ,
                            Func<T, TValue>     valueSelector   )
            where TKey : IComparable<TKey>
        {
            if (values == null)
            {
                return RedBlackTree<TKey, TValue>.Empty;
            }

            var result = RedBlackTree<TKey, TValue>.Empty;

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
        static double Log2 (this int v)
        {
            return Math.Log (v) / Math.Log (2);
        }

        static int ToInt (this double v)
        {
            return (int) Math.Round (v);
        }

        static void Test (this IEnumerable<int> values, string testRun)
        {
            Console.WriteLine ("Running test run: {0}", testRun);

            var vs = (values ?? new int[0]).ToArray ();
            var uvs = vs.Distinct ().OrderBy (v => v).ToArray ();

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
                    Console.WriteLine ("FAILED: {0} found in tree but wrong value {1} != {2}", v, vv, 2*v);
                }
            }

            var count = rbt.Count;
            if (uvs.Length != count)
            {
                Console.WriteLine ("FAILED: Length mismatch: {0} != {1}", uvs.Length, count);
            }

            var rbts = rbt.Select (kv => kv.Key).ToArray ();

            var missingKeys = uvs.Except (rbts).ToArray ();
            var extraKeys   = rbts.Except (uvs).ToArray ();

            if (missingKeys.Length > 0)
            {
                Console.WriteLine ("FAILED: #{0} missing keys", missingKeys.Length);
            }

            if (extraKeys.Length > 0)
            {
                Console.WriteLine ("FAILED: #{0} extra keys", extraKeys.Length);
            }

            var depth = rbt.Depth;
            var minDepth = count.Log2 ().ToInt ();
            var maxDepth = (2 * (count + 1).Log2 ()).ToInt ();

            if (minDepth > depth)
            {
                Console.WriteLine ("FAILED: minDepth ({0}) exceeds depth ({1})", minDepth, depth);
            }

            if (maxDepth < depth)
            {
                Console.WriteLine ("FAILED: maxDepth ({0}) is lower than depth ({1})", maxDepth, depth);
            }
        }

        static void Main (string[] args)
        {
            (new [] {31,41,59,26,53,58,97,93}).Test ("Manual");
            Enumerable.Range (1000, 1000).Test ("Sorted");

            var r = new Random (19740531);
            for (var iter = 0; iter < 10; ++iter)
            {
                var length = r.Next (1000, 2000);
                Enumerable.Range (0, length).Select (i => r.Next (0, 10000)).Test ("Generated: " + iter);
            }
        }
    }
}
