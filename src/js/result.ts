export type Result<T, E = Error> =
  | { status: "ok"; data: T }
  | { status: "err"; error: E };

export function resultOk<T, E>(data: T): Result<T, E> {
  return { status: "ok", data };
}

export function resultErr<T, E>(error: E): Result<T, E> {
  return { status: "err", error };
}

export function resultMap<T, N, E>(
  result: Result<T, E>,
  mapFn: (data: T) => N
): Result<N, E> {
  if (result.status === "ok") {
    return resultOk(mapFn(result.data));
  } else {
    return result;
  }
}

export function resultCallback<T, E>(
  result: Result<T, E>,
  callback: (data: T) => void | Promise<void>,
  errorCallback: ((error: E) => void | Promise<void>) | null = null
): void {
  if (result.status === "err") {
    void errorCallback?.(result.error);
  } else {
    void callback(result.data);
  }
}
