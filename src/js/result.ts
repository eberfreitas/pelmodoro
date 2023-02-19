export type Result<T, E = Error> =
  | { status: "ok"; data: T }
  | { status: "err"; error: E };

export function mapResult<T, N, E>(
  result: Result<T, E>,
  mapFn: (data: T) => N
): Result<N, E> {
  if (result.status === "ok") {
    return { status: "ok", data: mapFn(result.data) };
  } else {
    return result;
  }
}

export function resultOk<T, E>(data: T): Result<T, E> {
  return { status: "ok", data };
}

export function resultErr<T, E>(error: E): Result<T, E> {
  return { status: "err", error };
}
